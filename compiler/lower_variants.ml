open Core
open Anf
open Sexplib.Sexp
open Monomorphize
open Tail_call

(* TODO: Add type for lower_variants to remove variants *)

include Compiler_error.Pass (struct
    let name = "lower_variants"
  end)

type term_desc =
  | Atom of atom
  | Bop of Glsl.binary_op * atom * atom
  | Vec of int * atom list
  | Index of atom * int
  | Builtin of Glsl.builtin * atom list
  | App of string * atom list
  | If of atom * anf * anf
  | Record of atom list
  | Field of atom * string
  | Switch of atom * (Glsl.switch_case * anf) list

and term =
  { desc : term_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

(** TODO: Since we added Temp type in [anf], we should not need to use [set] anymore for lowering.
    This may also apply in the while loop case of [tail call] *)
and anf_desc =
  | Let of string * term * anf
  | Return of term
  | While of term * anf * anf
  | Set of string * atom * anf
  | Continue

and anf =
  { desc : anf_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

let rec sexp_of_term_desc : term_desc -> Sexp.t = function
  | Atom a -> sexp_of_atom a
  | Bop (op, l, r) ->
    List [ Atom (Glsl.string_of_binary_op op); sexp_of_atom l; sexp_of_atom r ]
  | Vec (n, ts) -> List (Atom ("vec" ^ Int.to_string n) :: List.map ts ~f:sexp_of_atom)
  | Index (t, i) -> List [ Atom "index"; sexp_of_atom t; Atom (Int.to_string i) ]
  | Builtin (b, ts) ->
    List (Atom (Glsl.string_of_builtin b) :: List.map ts ~f:sexp_of_atom)
  | App (f, args) -> List (Atom f :: List.map args ~f:sexp_of_atom)
  | If (c, t, e) -> List [ Atom "if"; sexp_of_atom c; sexp_of_anf t; sexp_of_anf e ]
  | Record ts -> List (Atom "record" :: List.map ts ~f:sexp_of_atom)
  | Field (t, f) -> List [ Atom "."; sexp_of_atom t; Atom f ]
  | Switch (tag, cases) ->
    let sexp_of_case (label, body) =
      let lbl =
        match label with
        | Glsl.Case i -> Int.to_string i
        | Glsl.Default -> "default"
      in
      List [ Atom lbl; sexp_of_anf body ]
    in
    List (Atom "switch" :: sexp_of_atom tag :: List.map cases ~f:sexp_of_case)

and sexp_of_term t = sexp_of_term_desc t.desc

and sexp_of_anf_desc = function
  | Let (v, bind, body) ->
    List [ Atom "let"; Atom v; sexp_of_term bind; sexp_of_anf body ]
  | Return t -> List [ Atom "return"; sexp_of_term t ]
  | While (cond, body, after) ->
    List [ Atom "while"; sexp_of_term cond; sexp_of_anf body; sexp_of_anf after ]
  | Set (v, bind, body) ->
    List [ Atom "set"; Atom v; sexp_of_atom bind; sexp_of_anf body ]
  | Continue -> Atom "continue"

and sexp_of_anf t = sexp_of_anf_desc t.desc

type top_desc =
  | Define of
      { name : string
      ; args : (string * ty) list
      ; body : anf
      ; ret_ty : Monomorphize.ty
      }
  | Const of string * anf
  | Extern of string
  | TypeDef of string * type_decl

let sexp_of_top_desc = function
  | Define { name; args; body; ret_ty = _ } ->
    let args_sexp = List.map args ~f:(fun (v, ty) -> List [ Atom v; sexp_of_ty ty ]) in
    List
      [ Atom "Define"
      ; List [ Atom "name"; Atom name ]
      ; List [ Atom "args"; List args_sexp ]
      ; List [ Atom "body"; sexp_of_anf body ]
      ]
  | Const (name, term) -> List [ Atom "Const"; Atom name; sexp_of_anf term ]
  | Extern name -> List [ Atom "Extern"; Atom name ]
  | TypeDef (name, decl) -> List [ Atom "TypeDef"; Atom name; sexp_of_type_decl decl ]
;;

type top =
  { desc : top_desc
  ; ty : Monomorphize.ty
  ; loc : Lexer.loc
  }

let sexp_of_top t =
  List [ sexp_of_top_desc t.desc; Atom ":"; Monomorphize.sexp_of_ty t.ty ]
;;

type t = Program of top list

let sexp_of_t (Program tops) = List (Atom "Program" :: List.map tops ~f:sexp_of_top)

type type_env = type_decl String.Map.t

let rec lower_ty (ty : ty) : ty =
  match ty with
  | TyVariant s -> TyRecord s
  | TyArrow (a, b) -> TyArrow (lower_ty a, lower_ty b)
  | TyVec (n, t) -> TyVec (n, lower_ty t)
  | TyFloat | TyInt | TyBool | TyRecord _ -> ty
;;

let find_tag ~loc (ctors : (string * ty list) list) (ctor : string) : int =
  match List.findi ctors ~f:(fun _ (c, _) -> String.equal c ctor) with
  | Some (i, _) -> i
  | None -> raise "unknown ctor" ~loc ~d:[%message (ctor : string)]
;;

(** Lays out a variant constructor's args into the flat record layout used after *)
let flatten_variant_args ~loc ~ctor ~args (ctors : (string * ty list) list) : atom list =
  let placeholder ty : atom = { desc = Temp; ty; loc } in
  List.concat_map ctors ~f:(fun (c, arg_tys) ->
    if String.equal c ctor then args else List.map arg_tys ~f:placeholder)
;;

let rec map_last_return (k : term -> anf) (anf : anf) : anf =
  match anf.desc with
  | Return term -> k term
  | Let (v, b, t) -> { anf with desc = Let (v, b, map_last_return k t) }
  | While (c, b, t) -> { anf with desc = While (c, b, map_last_return k t) }
  | Set (v, a, t) -> { anf with desc = Set (v, a, map_last_return k t) }
  | Continue -> anf
;;

(* ================ Sub Occurrence Helpers ======================== *)

(** A sub-occurrence introduced when destructuring a head pattern *)
type sub_occ =
  { name : string
  ; atom : Anf.atom
  ; bind : term
  }

(** Chain of [Let v = bind_term] wrapping [body] *)
let bind_lets ~loc (binds : (string * term) list) (body : anf) : anf =
  List.fold_right binds ~init:body ~f:(fun (v, bind) acc ->
    ({ desc = Let (v, bind, acc); ty = acc.ty; loc } : anf))
;;

(** If [pat] is [PatVar v], wrap [body] in [Let v = occ in body]; else identity *)
let bind_pat_var_tc ~loc ~(occ : Anf.atom) (pat : Frontend.pat) (body : Tail_call.anf)
  : Tail_call.anf
  =
  match pat with
  | PatVar v ->
    let bind : Tail_call.term = { desc = Atom occ; ty = occ.ty; loc } in
    { desc = Let (v, bind, body); ty = body.ty; loc }
  | _ -> body
;;

(** Post-lowering version of [bind_pat_var_tc] *)
let bind_pat_var ~loc ~(occ : Anf.atom) (pat : Frontend.pat) (body : anf) : anf =
  match pat with
  | PatVar v ->
    let bind : term = { desc = Atom occ; ty = occ.ty; loc } in
    { desc = Let (v, bind, body); ty = body.ty; loc }
  | _ -> body
;;

(* ================ Head Abstraction ======================== *)

(** Discriminator for a pivot column, gets arity and subocc binding. *)
type head =
  | HBool of bool
  | HInt of int
  | HFloat of float
  | HCtor of string * (ty list[@equal.ignore])
  | HBracket of int * (ty[@equal.ignore])
  | HRecord of ((string * ty) list[@equal.ignore])
[@@deriving equal]

let head_sub_tys = function
  | HBool _ | HInt _ | HFloat _ -> []
  | HCtor (_, ts) -> ts
  | HBracket (n, x) -> List.init n ~f:(Fn.const x)
  | HRecord fs -> List.map fs ~f:snd
;;

let lookup_variant_ctors ~loc (tenv : type_env) name =
  match Map.find tenv name with
  | Some (VariantDecl cs) -> cs
  | _ -> raise "unknown variant in match" ~loc ~d:[%message (name : string)]
;;

let lookup_record_fields ~loc (tenv : type_env) name =
  match Map.find tenv name with
  | Some (RecordDecl fs) -> fs
  | _ -> raise "unknown struct in match" ~loc ~d:[%message (name : string)]
;;

let head_of_pat ~loc ~(tenv : type_env) ~(col_ty : ty) : Frontend.pat -> head option
  = function
  | PatWildcard | PatVar _ -> None
  | PatLitBool b -> Some (HBool b)
  | PatLitInt n -> Some (HInt n)
  | PatLitFloat f -> Some (HFloat f)
  | PatCtor (c, _) ->
    let arg_tys =
      match col_ty with
      | TyVariant name ->
        List.Assoc.find (lookup_variant_ctors ~loc tenv name) ~equal:String.equal c
        |> Option.value ~default:[]
      | _ -> []
    in
    Some (HCtor (c, arg_tys))
  | PatBracket _ ->
    (match col_ty with
     | TyVec (n, elem) -> Some (HBracket (n, elem))
     | _ -> Some (HBracket (0, TyInt)))
  | PatRecord _ ->
    let fs =
      match col_ty with
      | TyRecord name -> lookup_record_fields ~loc tenv name
      | _ -> []
    in
    Some (HRecord fs)
;;

(** Sub-patterns of [pat] under [h]: [Some args] if [pat]'s head equals [h]
    (wildcards expanded to head arity), [None] if [pat] would be filtered out
    when specializing on [h]. *)
let pat_args ~(h : head) : Frontend.pat -> Frontend.pat list option = function
  | PatWildcard | PatVar _ ->
    Some (List.map (head_sub_tys h) ~f:(Fn.const Frontend.PatWildcard))
  | PatCtor (c, args) ->
    (match h with
     | HCtor (c', _) when String.equal c c' -> Some args
     | _ -> None)
  | PatLitBool b ->
    (match h with
     | HBool b' when Bool.equal b b' -> Some []
     | _ -> None)
  | PatLitInt n ->
    (match h with
     | HInt n' when Int.equal n n' -> Some []
     | _ -> None)
  | PatLitFloat f ->
    (match h with
     | HFloat f' when Float.equal f f' -> Some []
     | _ -> None)
  | PatBracket pats ->
    (match h with
     | HBracket _ -> Some pats
     | _ -> None)
  | PatRecord (fields, _) ->
    (match h with
     | HRecord struct_fields ->
       Some
         (List.map struct_fields ~f:(fun (fname, _) ->
            List.Assoc.find fields ~equal:String.equal fname
            |> Option.value ~default:Frontend.PatWildcard))
     | _ -> None)
;;

(** Distinct heads observed in the first column of [rows], in first-seen order. *)
let column_heads ~loc ~tenv ~col_ty (rows : (Frontend.pat list * _) list) : head list =
  List.filter_map rows ~f:(function
    | p :: _, _ -> head_of_pat ~loc ~tenv ~col_ty p
    | [], _ -> None)
  |> List.fold ~init:[] ~f:(fun acc h ->
    if List.mem acc h ~equal:equal_head then acc else h :: acc)
  |> List.rev
;;

let signature_heads ~loc ~(tenv : type_env) : ty -> head list option = function
  | TyBool -> Some [ HBool true; HBool false ]
  | TyVariant name ->
    Some
      (lookup_variant_ctors ~loc tenv name |> List.map ~f:(fun (c, ts) -> HCtor (c, ts)))
  | TyVec (n, elem) -> Some [ HBracket (n, elem) ]
  | TyRecord name -> Some [ HRecord (lookup_record_fields ~loc tenv name) ]
  | TyInt | TyFloat | TyArrow _ -> None
;;

let make_sub_occ ~loc ~hint ~ty ~bind_desc : sub_occ =
  let name = Utils.fresh ("_lv_" ^ hint) in
  { name
  ; atom = { desc = Var name; ty; loc }
  ; bind = { desc = bind_desc; ty = lower_ty ty; loc }
  }
;;

(** Sub-occurrences introduced by destructuring a head pattern. *)
let sub_occs_for_head ~loc ~(occ : Anf.atom) : head -> sub_occ list = function
  | HBool _ | HInt _ | HFloat _ -> []
  | HCtor (c, arg_tys) ->
    List.mapi arg_tys ~f:(fun i ty ->
      let f = [%string "%{c}_%{i#Int}"] in
      make_sub_occ ~loc ~hint:f ~ty ~bind_desc:(Field (occ, f)))
  | HBracket (n, ty) ->
    List.init n ~f:(fun i ->
      make_sub_occ ~loc ~hint:[%string "v%{i#Int}"] ~ty ~bind_desc:(Index (occ, i)))
  | HRecord fs ->
    List.map fs ~f:(fun (f, ty) ->
      make_sub_occ ~loc ~hint:("r_" ^ f) ~ty ~bind_desc:(Field (occ, f)))
;;

(** Whether [heads] cover the full value domain of [col_ty] *)
let signature_complete ~loc ~tenv ~col_ty ~heads : bool =
  match signature_heads ~loc ~tenv col_ty with
  | None -> false
  | Some sign -> List.for_all sign ~f:(List.mem heads ~equal:equal_head)
;;

(* ================ Lowering Variants ======================== *)

let rec lower_term (tenv : type_env) (term : Tail_call.term) : term =
  let pure desc = ({ desc; ty = lower_ty term.ty; loc = term.loc } : term) in
  match term.desc with
  | Atom a -> pure (Atom a)
  | Bop (op, l, r) -> pure (Bop (op, l, r))
  | Vec (n, ts) -> pure (Vec (n, ts))
  | Index (t, i) -> pure (Index (t, i))
  | Builtin (b, ts) -> pure (Builtin (b, ts))
  | App (f, args) -> pure (App (f, args))
  | Record args -> pure (Record args)
  | Field (a, f) -> pure (Field (a, f))
  | If (c, t, e) -> pure (If (c, lower_anf tenv t, lower_anf tenv e))
  | Variant (ctor, args) ->
    let loc = term.loc in
    let ty_name =
      match term.ty with
      | TyVariant n -> n
      | _ -> raise "expected variant type" ~loc ~d:[%message (term : Tail_call.term)]
    in
    let ctors = lookup_variant_ctors ~loc tenv ty_name in
    let tag_atom : atom = { desc = Int (find_tag ~loc ctors ctor); ty = TyInt; loc } in
    pure (Record (tag_atom :: flatten_variant_args ~loc ~ctor ~args ctors))
  | Match _ -> raise "match should be handled in lower_anf" ~loc:term.loc

and lower_anf (tenv : type_env) (anf : Tail_call.anf) : anf =
  let make desc : anf = { desc; ty = lower_ty anf.ty; loc = anf.loc } in
  match anf.desc with
  | Let (v, { desc = Match (scrut, cases); ty; _ }, tail) ->
    let tail = lower_anf tenv tail in
    lower_match tenv scrut cases ty anf.loc (fun t -> make (Let (v, t, tail)))
  | Return { desc = Match (scrut, cases); ty; _ } ->
    lower_match tenv scrut cases ty anf.loc (fun t -> make (Return t))
  | Let (v, term, tail) ->
    let term = lower_term tenv term in
    let tail = lower_anf tenv tail in
    make (Let (v, term, tail))
  | Return term -> make (Return (lower_term tenv term))
  | While (cond, body, after) ->
    let cond = lower_term tenv cond in
    let body = lower_anf tenv body in
    let after = lower_anf tenv after in
    make (While (cond, body, after))
  | Set (v, a, tail) -> make (Set (v, a, lower_anf tenv tail))
  | Continue -> make Continue

and lower_match
      (tenv : type_env)
      (scrut : Anf.atom)
      (cases : (Frontend.pat * Tail_call.anf) list)
      (result_ty : ty)
      (loc : Lexer.loc)
      (k : term -> anf)
  : anf
  =
  let result_ty = lower_ty result_ty in
  let return_term (t : term) : anf = { desc = Return t; ty = t.ty; loc } in
  let pivot_to_front i xs =
    if i = 0
    then xs
    else (
      let before, after = List.split_n xs i in
      match after with
      | x :: rest -> x :: (before @ rest)
      | [] -> raise "internal: pivot index out of range" ~loc)
  in
  let default_on ~occ rows =
    Pattern_match.Matrix.default ~on_wild_head:(bind_pat_var_tc ~loc ~occ) rows
  in
  let emit_bool ~(occ : Anf.atom) ~branches ~default =
    let find h = List.Assoc.find branches ~equal:equal_head h in
    let arm b =
      match find (HBool b), default with
      | Some body, _ -> body
      | None, Some d -> d
      | None, None -> raise "bool match: missing arm and no default" ~loc
    in
    return_term { desc = If (occ, arm true, arm false); ty = result_ty; loc }
  in
  let emit_int ~(occ : Anf.atom) ~branches ~default =
    let cases =
      List.map branches ~f:(fun (h, b) ->
        match h with
        | HInt n -> Glsl.Case n, b
        | _ -> raise "internal: non-int head under TyInt" ~loc)
    in
    let tail = Option.value_map default ~default:[] ~f:(fun d -> [ Glsl.Default, d ]) in
    return_term { desc = Switch (occ, cases @ tail); ty = result_ty; loc }
  in
  let emit_float ~(occ : Anf.atom) ~branches ~default =
    let init =
      match default with
      | Some d -> d
      | None -> raise "float match: missing catch-all" ~loc
    in
    List.fold_right branches ~init ~f:(fun (h, body) acc ->
      let f_val =
        match h with
        | HFloat f -> f
        | _ -> raise "internal: non-float head under TyFloat" ~loc
      in
      let cmp_v = Utils.fresh "_lv_cmp" in
      let cmp_atom : atom = { desc = Var cmp_v; ty = TyBool; loc } in
      let f_atom : atom = { desc = Float f_val; ty = TyFloat; loc } in
      let cmp : term = { desc = Bop (Glsl.Eq, occ, f_atom); ty = TyBool; loc } in
      let if_t : term = { desc = If (cmp_atom, body, acc); ty = result_ty; loc } in
      { desc = Let (cmp_v, cmp, return_term if_t); ty = result_ty; loc })
  in
  let emit_variant ~ty_name ~(occ : Anf.atom) ~branches ~default =
    let ctors = lookup_variant_ctors ~loc tenv ty_name in
    let n = List.length branches in
    let has_default = Option.is_some default in
    let cases =
      List.mapi branches ~f:(fun i (h, body) ->
        let ctor =
          match h with
          | HCtor (c, _) -> c
          | _ -> raise "internal: non-ctor head under TyVariant" ~loc
        in
        let is_last = i = n - 1 && not has_default in
        let label : Glsl.switch_case =
          if is_last then Default else Case (find_tag ~loc ctors ctor)
        in
        label, body)
    in
    let default_cases =
      Option.value_map default ~default:[] ~f:(fun d -> [ Glsl.Default, d ])
    in
    match cases, default_cases with
    | [ (_, only) ], [] -> only
    | _ ->
      let tag_v = Utils.fresh "_lv_tag" in
      let tag_atom : atom = { desc = Var tag_v; ty = TyInt; loc } in
      let tag : term = { desc = Field (occ, "tag"); ty = TyInt; loc } in
      let sw : term =
        { desc = Switch (tag_atom, cases @ default_cases); ty = result_ty; loc }
      in
      { desc = Let (tag_v, tag, return_term sw); ty = result_ty; loc }
  in
  let emit_switch ~(occ : Anf.atom) ~(col_ty : ty) ~branches ~default : anf =
    match col_ty with
    | TyBool -> emit_bool ~occ ~branches ~default
    | TyInt -> emit_int ~occ ~branches ~default
    | TyFloat -> emit_float ~occ ~branches ~default
    | TyVec _ | TyRecord _ ->
      (match branches with
       | [ (_, body) ] -> body
       | _ -> raise "vec/record match: expected exactly one head" ~loc)
    | TyVariant ty_name -> emit_variant ~ty_name ~occ ~branches ~default
    | TyArrow _ -> raise "cannot match on arrow type" ~loc
  in
  let rec go (occs : Anf.atom list) (rows : (Frontend.pat list * Tail_call.anf) list)
    : anf
    =
    match Pattern_match.Matrix.classify rows, occs with
    | `Empty, _ -> raise "unreachable: typecheck should have checked exhaustiveness" ~loc
    | `Leaf (pats, body), _ ->
      List.fold2_exn pats occs ~init:(lower_anf tenv body) ~f:(fun acc pat occ ->
        bind_pat_var ~loc ~occ pat acc)
    | `Pivot _, [] -> raise "unreachable: pivot exhaustiveness" ~loc
    | `Pivot col_idx, _ :: _ ->
      let occ, rest =
        match pivot_to_front col_idx occs with
        | h :: t -> h, t
        | [] -> raise "unreachable: pivot_to_front preserves size" ~loc
      in
      let rows =
        List.map rows ~f:(fun (pats, body) -> pivot_to_front col_idx pats, body)
      in
      let col_ty = occ.ty in
      let heads = column_heads ~loc ~tenv ~col_ty rows in
      let branches =
        List.map heads ~f:(fun h ->
          let arity = List.length (head_sub_tys h) in
          let spec =
            Pattern_match.Matrix.specialize
              ~on_wild_head:(bind_pat_var_tc ~loc ~occ)
              ~arity
              ~expand:(pat_args ~h)
              rows
          in
          let sub_occs = sub_occs_for_head ~loc ~occ h in
          let sub_atoms = List.map sub_occs ~f:(fun s -> s.atom) in
          let binds = List.map sub_occs ~f:(fun s -> s.name, s.bind) in
          h, go (sub_atoms @ rest) spec |> bind_lets ~loc binds)
      in
      let default =
        if signature_complete ~loc ~tenv ~col_ty ~heads
        then None
        else Some (go rest (default_on ~occ rows))
      in
      emit_switch ~occ ~col_ty ~branches ~default
  in
  cases
  |> List.map ~f:(fun (pat, body) -> [ pat ], body)
  |> go [ scrut ]
  |> map_last_return k
;;

let lower_top (tenv : type_env) (top : Tail_call.top) : top =
  let pure desc = ({ desc; ty = lower_ty top.ty; loc = top.loc } : top) in
  match top.desc with
  | TypeDef (name, VariantDecl ctors) ->
    let flat_fields =
      List.concat_map ctors ~f:(fun (ctor, arg_tys) ->
        List.mapi arg_tys ~f:(fun i t -> [%string "%{ctor}_%{i#Int}"], lower_ty t))
    in
    pure (TypeDef (name, RecordDecl (("tag", TyInt) :: flat_fields)))
  | TypeDef (name, RecordDecl fields) ->
    let fields = List.map fields ~f:(Tuple2.map_snd ~f:lower_ty) in
    pure (TypeDef (name, RecordDecl fields))
  | Define { name; args; body; ret_ty } ->
    let args = List.map args ~f:(Tuple2.map_snd ~f:lower_ty) in
    let ret_ty = lower_ty ret_ty in
    pure (Define { name; args; body = lower_anf tenv body; ret_ty })
  | Extern v -> pure (Extern v)
  | Const (name, body) -> pure (Const (name, lower_anf tenv body))
;;

let lower (Program tops : Tail_call.t) : t Compiler_error.t =
  try_with (fun () ->
    let tenv =
      tops
      |> List.filter_map ~f:(fun top ->
        match top.desc with
        | TypeDef (s, decl) -> Some (s, decl)
        | Define _ | Extern _ | Const _ -> None)
      |> String.Map.of_alist_or_error
      |> of_or_error
      |> ok_exn
    in
    Program (List.map tops ~f:(lower_top tenv)))
;;

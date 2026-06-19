open Core
open Sexplib.Sexp

include Compiler_error.Pass (struct
    let name = "lower_variants"
  end)

(* ===== Types ===== *)

type ty =
  | TyFloat
  | TyInt
  | TyBool
  | TyVec of int * ty
  | TyArrow of ty * ty
  | TyRecord of string
  | TySampler
[@@deriving equal]

let rec sexp_of_ty = function
  | TyFloat -> Atom "float"
  | TyInt -> Atom "int"
  | TyBool -> Atom "bool"
  | TyVec (i, t) -> List [ Atom "vec"; Atom (Int.to_string i); sexp_of_ty t ]
  | TyArrow (t, t') -> List [ sexp_of_ty t; Atom "->"; sexp_of_ty t' ]
  | TyRecord s -> Atom s
  | TySampler -> Atom "sampler"
;;

type type_decl = RecordDecl of (string * ty) list [@@deriving sexp_of]

let rec lower_ty (ty : Lower_tuples.ty) : ty =
  match ty with
  | TyVariant s -> TyRecord s
  | TyArrow (a, b) -> TyArrow (lower_ty a, lower_ty b)
  | TyVec (n, t) -> TyVec (n, lower_ty t)
  | TyFloat -> TyFloat
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyRecord s -> TyRecord s
  | TySampler -> TySampler
;;

(* ======= Variant payload slot sharing ======= *)

(* GLSL-identifier encoding of a slot type *)
let field_name (ty : ty) (k : int) : string =
  let rec ty_short (ty : ty) : string =
    match ty with
    | TyFloat -> "f"
    | TyInt -> "i"
    | TyBool -> "b"
    | TyVec (n, t) -> [%string "v%{n#Int}%{ty_short t}"]
    | TyRecord s -> "r" ^ s
    | TyArrow _ -> "fn"
    | TySampler -> "samp"
  in
  [%string "p%{ty_short ty}_%{k#Int}"]
;;

(** Slot [(name, type)] for each argument of one constructor *)
let assign (arg_tys : Lower_tuples.ty list) : (string * ty) list =
  List.folding_map arg_tys ~init:[] ~f:(fun seen raw ->
    let ty = lower_ty raw in
    let k = List.count seen ~f:(equal_ty ty) in
    ty :: seen, (field_name ty k, ty))
;;

(* Needed union of every constructor's slots *)
let slot_layout (ctors : (string * Lower_tuples.ty list) list) : (string * ty) list =
  ctors
  |> List.map ~f:snd
  |> List.concat_map ~f:assign
  |> List.stable_dedup ~compare:(fun (n1, _) (n2, _) -> String.compare n1 n2)
;;

type term_desc =
  | Var of string
  | Float of float
  | Int of int
  | Bool of bool
  | Vec of int * term list
  | App of string * term list
  | Let of string * term * term
  | If of term * term * term
  | Bop of Glsl.binary_op * term * term
  | Index of term * int
  | Builtin of Glsl.builtin * term list
  | Record of term list
  | Init_struct of (string * term) list
  | Field of term * string
  | Switch of term * (Glsl.switch_case * term) list

and term =
  { desc : term_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

let rec sexp_of_term_desc : term_desc -> Sexp.t = function
  | Var v -> Atom v
  | Float f -> Atom (Float.to_string f)
  | Int i -> Atom (Int.to_string i)
  | Bool b -> Atom (Bool.to_string b)
  | Vec (n, ts) -> List (Atom ("vec" ^ Int.to_string n) :: List.map ts ~f:sexp_of_term)
  | App (f, args) -> List (Atom f :: List.map args ~f:sexp_of_term)
  | Let (v, bind, body) ->
    List [ Atom "let"; Atom v; sexp_of_term bind; sexp_of_term body ]
  | If (c, t, e) -> List [ Atom "if"; sexp_of_term c; sexp_of_term t; sexp_of_term e ]
  | Bop (op, l, r) ->
    List [ Atom (Glsl.string_of_binary_op op); sexp_of_term l; sexp_of_term r ]
  | Index (t, i) -> List [ Atom "index"; sexp_of_term t; Atom (Int.to_string i) ]
  | Builtin (b, ts) ->
    List (Atom (Glsl.string_of_builtin b) :: List.map ts ~f:sexp_of_term)
  | Record ts -> List (Atom "record" :: List.map ts ~f:sexp_of_term)
  | Init_struct fields ->
    let sexp_of_field (f, t) = List [ Atom f; sexp_of_term t ] in
    List (Atom "init_struct" :: List.map fields ~f:sexp_of_field)
  | Field (t, f) -> List [ Atom "."; sexp_of_term t; Atom f ]
  | Switch (tag, cases) ->
    let sexp_of_case (label, body) =
      let lbl =
        match label with
        | Glsl.Case i -> Int.to_string i
        | Glsl.Default -> "default"
      in
      List [ Atom lbl; sexp_of_term body ]
    in
    List (Atom "switch" :: sexp_of_term tag :: List.map cases ~f:sexp_of_case)

and sexp_of_term t = sexp_of_term_desc t.desc

type top_desc =
  | Define of
      { name : string
      ; recur : Frontend.recur
      ; args : (string * ty) list
      ; body : term
      ; ret_ty : ty
      }
  | Const of string * term
  | Extern of string
  | TypeDef of string * type_decl

let sexp_of_top_desc = function
  | Define { name; recur; args; body; ret_ty = _ } ->
    let args_sexp = List.map args ~f:(fun (v, ty) -> List [ Atom v; sexp_of_ty ty ]) in
    List
      [ Atom "Define"
      ; Frontend.sexp_of_recur recur
      ; List [ Atom "name"; Atom name ]
      ; List [ Atom "args"; List args_sexp ]
      ; List [ Atom "body"; sexp_of_term body ]
      ]
  | Const (name, term) -> List [ Atom "Const"; Atom name; sexp_of_term term ]
  | Extern name -> List [ Atom "Extern"; Atom name ]
  | TypeDef (name, decl) -> List [ Atom "TypeDef"; Atom name; sexp_of_type_decl decl ]
;;

type top =
  { desc : top_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

let sexp_of_top t = List [ sexp_of_top_desc t.desc; Atom ":"; sexp_of_ty t.ty ]

type t = Program of top list

let sexp_of_t (Program tops) = List (Atom "Program" :: List.map tops ~f:sexp_of_top)

(* ================ Lowering =================== *)

type type_env = Lower_tuples.type_decl String.Map.t

let find_tag ~loc (ctors : (string * Lower_tuples.ty list) list) (ctor : string) : int =
  match List.findi ctors ~f:(fun _ (c, _) -> String.equal c ctor) with
  | Some (i, _) -> i
  | None -> raise "unknown ctor" ~loc ~d:[%message (ctor : string)]
;;

(** The (slot-name, value) pairs initialized by constructing [ctor] with [args] *)
let variant_fields ~loc ~ctor ~(args : term list) ctors : (string * term) list =
  let tag : term = { desc = Int (find_tag ~loc ctors ctor); ty = TyInt; loc } in
  let slot_names =
    ctors
    |> List.Assoc.find ~equal:String.equal ctor
    |> Option.value ~default:[]
    |> assign
    |> List.map ~f:fst
  in
  ("tag", tag) :: List.zip_exn slot_names args
;;

(* If [pat] is [PatVar v], wrap [body] in [let v = occ in body] *)
let bind_pat_var ~loc ~occ_name ~occ_ty (pat : Frontend.pat) (body : term) : term =
  match pat with
  | PatVar v ->
    let bind : term = { desc = Var occ_name; ty = occ_ty; loc } in
    { desc = Let (v, bind, body); ty = body.ty; loc }
  | _ -> body
;;

let bind_lets ~loc (binds : (string * term) list) (body : term) : term =
  List.fold_right binds ~init:body ~f:(fun (v, bind) acc ->
    ({ desc = Let (v, bind, acc); ty = acc.ty; loc } : term))
;;

(* Parallels [Pattern_match.head] *)
type head =
  | HBool of bool
  | HInt of int
  | HFloat of float
  | HCtor of string
  | HBracket of int
  | HRecord
[@@deriving equal]

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

let head_of_pat ~loc : Frontend.pat -> head option = function
  | PatWildcard | PatVar _ -> None
  | PatLitBool b -> Some (HBool b)
  | PatLitInt n -> Some (HInt n)
  | PatLitFloat f -> Some (HFloat f)
  | PatCtor (c, _) -> Some (HCtor c)
  | PatBracket pats -> Some (HBracket (List.length pats))
  | PatRecord _ -> Some HRecord
  | PatTuple _ -> raise "unexpected PatTuple after lower_tuples" ~loc
;;

let column_heads ~loc (rows : (Frontend.pat list * _) list) : head list =
  List.filter_map rows ~f:(function
    | p :: _, _ -> head_of_pat ~loc p
    | [], _ -> None)
  |> List.fold ~init:[] ~f:(fun acc h ->
    if List.mem acc h ~equal:equal_head then acc else h :: acc)
  |> List.rev
;;

let signature_heads ~loc ~(tenv : type_env) : Lower_tuples.ty -> head list option
  = function
  | TyBool -> Some [ HBool true; HBool false ]
  | TyVariant name ->
    Some (lookup_variant_ctors ~loc tenv name |> List.map ~f:(fun (c, _) -> HCtor c))
  | TyVec (n, _) -> Some [ HBracket n ]
  | TyRecord _ -> Some [ HRecord ]
  | TyInt | TyFloat | TyArrow _ | TySampler -> None
;;

(* Do [heads] cover the full value domain of [col_ty]? *)
let signature_complete ~loc ~tenv ~col_ty ~heads =
  match signature_heads ~loc ~tenv col_ty with
  | None -> false
  | Some sign -> List.for_all sign ~f:(List.mem heads ~equal:equal_head)
;;

(* Variable handle + type *)
type occ =
  { name : string
  ; ty : Lower_tuples.ty
  ; loc : Lexer.loc
  }

let occ_to_term (occ : occ) : term =
  { desc = Var occ.name; ty = lower_ty occ.ty; loc = occ.loc }
;;

let rec lower_term (tenv : type_env) (term : Lambda_lift.term) : term =
  let lty = lower_ty term.ty in
  let pure desc = ({ desc; ty = lty; loc = term.loc } : term) in
  match term.desc with
  | Var v -> pure (Var v)
  | Float f -> pure (Float f)
  | Int i -> pure (Int i)
  | Bool b -> pure (Bool b)
  | Vec (n, ts) -> pure (Vec (n, List.map ts ~f:(lower_term tenv)))
  | App (f, args) ->
    let name =
      match f.desc with
      | Var v -> v
      | _ -> raise "app function must be a variable" ~loc:term.loc
    in
    pure (App (name, List.map args ~f:(lower_term tenv)))
  | Let (v, bind, body) -> pure (Let (v, lower_term tenv bind, lower_term tenv body))
  | If (c, t, e) -> pure (If (lower_term tenv c, lower_term tenv t, lower_term tenv e))
  | Bop (op, l, r) -> pure (Bop (op, lower_term tenv l, lower_term tenv r))
  | Index (t, i) -> pure (Index (lower_term tenv t, i))
  | Builtin (b, ts) -> pure (Builtin (b, List.map ts ~f:(lower_term tenv)))
  | Record ts -> pure (Record (List.map ts ~f:(lower_term tenv)))
  | Field (t, f) -> pure (Field (lower_term tenv t, f))
  | Variant (ctor, args) ->
    let loc = term.loc in
    let ty_name =
      match term.ty with
      | TyVariant n -> n
      | _ -> raise "expected variant type" ~loc
    in
    let ctors = lookup_variant_ctors ~loc tenv ty_name in
    let args = List.map args ~f:(lower_term tenv) in
    pure (Init_struct (variant_fields ~loc ~ctor ~args ctors))
  | Match (scrut, cases) ->
    let scrut_lowered = lower_term tenv scrut in
    let cases = List.map cases ~f:(fun (pat, body) -> pat, lower_term tenv body) in
    lower_match tenv ~scrut_pre_ty:scrut.ty scrut_lowered cases lty term.loc

and lower_match
      (tenv : type_env)
      ~(scrut_pre_ty : Lower_tuples.ty)
      (scrut : term)
      (cases : (Frontend.pat * term) list)
      (result_ty : ty)
      (loc : Lexer.loc)
  : term
  =
  let mk ty desc : term = { desc; ty; loc } in
  let pivot_to_front i xs =
    let before, after = List.split_n xs i in
    match after with
    | x :: rest -> x :: (before @ rest)
    | [] -> raise "internal: pivot index out of range" ~loc
  in
  let bind_at ~occ pat body =
    bind_pat_var ~loc ~occ_name:occ.name ~occ_ty:(lower_ty occ.ty) pat body
  in
  let ctor_arg_tys ~col_ty c =
    match (col_ty : Lower_tuples.ty) with
    | TyVariant name ->
      List.Assoc.find (lookup_variant_ctors ~loc tenv name) ~equal:String.equal c
      |> Option.value ~default:[]
    | _ -> []
  in
  let record_fields ~col_ty =
    match (col_ty : Lower_tuples.ty) with
    | TyRecord name -> lookup_record_fields ~loc tenv name
    | _ -> []
  in
  let head_sub_tys ~col_ty : head -> Lower_tuples.ty list = function
    | HBool _ | HInt _ | HFloat _ -> []
    | HCtor c -> ctor_arg_tys ~col_ty c
    | HBracket n ->
      let elem =
        match col_ty with
        | TyVec (_, e) -> e
        | _ -> Lower_tuples.TyInt
      in
      List.init n ~f:(Fn.const elem)
    | HRecord -> record_fields ~col_ty |> List.map ~f:snd
  in
  let pat_args ~col_ty ~h : Frontend.pat -> Frontend.pat list option = function
    | PatWildcard | PatVar _ ->
      Some (List.map (head_sub_tys ~col_ty h) ~f:(Fn.const Frontend.PatWildcard))
    | PatCtor (c, args) -> Option.some_if (equal_head h (HCtor c)) args
    | PatLitBool b -> Option.some_if (equal_head h (HBool b)) []
    | PatLitInt n -> Option.some_if (equal_head h (HInt n)) []
    | PatLitFloat f -> Option.some_if (equal_head h (HFloat f)) []
    | PatBracket pats ->
      (match h with
       | HBracket _ -> Some pats
       | _ -> None)
    | PatRecord (fields, _) when equal_head h HRecord ->
      Some
        (record_fields ~col_ty
         |> List.map ~f:(fun (fname, _) ->
           List.Assoc.find fields ~equal:String.equal fname
           |> Option.value ~default:Frontend.PatWildcard))
    | PatRecord _ -> None
    | PatTuple _ -> raise "unexpected PatTuple after lower_tuples" ~loc
  in
  let sub_occs_for_head ~(occ : occ) (h : head) : (occ * term) list =
    let parent = occ_to_term occ in
    let project ~hint ~bind_desc ty : occ * term =
      let name = Utils.fresh ("_lv_" ^ hint) in
      { name; ty; loc }, mk (lower_ty ty) bind_desc
    in
    match h with
    | HBool _ | HInt _ | HFloat _ -> []
    | HCtor c ->
      let arg_tys = ctor_arg_tys ~col_ty:occ.ty c in
      List.zip_exn arg_tys (assign arg_tys)
      |> List.map ~f:(fun (ty, (f, _)) ->
        project ~hint:f ~bind_desc:(Field (parent, f)) ty)
    | HBracket n ->
      let elem =
        match occ.ty with
        | TyVec (_, e) -> e
        | _ -> Lower_tuples.TyInt
      in
      List.init n ~f:(fun i ->
        let hint = [%string "v%{i#Int}"] in
        project ~hint ~bind_desc:(Index (parent, i)) elem)
    | HRecord ->
      record_fields ~col_ty:occ.ty
      |> List.map ~f:(fun (f, ty) ->
        project ~hint:("r_" ^ f) ~bind_desc:(Field (parent, f)) ty)
  in
  let emit_bool ~(occ : occ) ~branches ~default : term =
    let arm b =
      match List.Assoc.find branches ~equal:equal_head (HBool b), default with
      | Some body, _ | None, Some body -> body
      | None, None -> raise "bool match: missing arm and no default" ~loc
    in
    mk result_ty (If (occ_to_term occ, arm true, arm false))
  in
  let emit_int ~(occ : occ) ~branches ~default : term =
    let cases =
      List.map branches ~f:(function
        | HInt n, b -> Glsl.Case n, b
        | _ -> raise "internal: non-int head under TyInt" ~loc)
    in
    let tail = Option.value_map default ~default:[] ~f:(fun d -> [ Glsl.Default, d ]) in
    mk result_ty (Switch (occ_to_term occ, cases @ tail))
  in
  let emit_float ~(occ : occ) ~branches ~default : term =
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
      let cmp = mk TyBool (Bop (Glsl.Eq, occ_to_term occ, mk TyFloat (Float f_val))) in
      let if_t = mk result_ty (If (mk TyBool (Var cmp_v), body, acc)) in
      mk result_ty (Let (cmp_v, cmp, if_t)))
  in
  let emit_variant ~ty_name ~(occ : occ) ~branches ~default : term =
    let ctors = lookup_variant_ctors ~loc tenv ty_name in
    let n = List.length branches in
    let has_default = Option.is_some default in
    let cases =
      List.mapi branches ~f:(fun i (h, body) ->
        let ctor =
          match h with
          | HCtor c -> c
          | _ -> raise "internal: non-ctor head under TyVariant" ~loc
        in
        let label : Glsl.switch_case =
          if i = n - 1 && not has_default
          then Default
          else Case (find_tag ~loc ctors ctor)
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
      let tag = mk TyInt (Field (occ_to_term occ, "tag")) in
      let sw = mk result_ty (Switch (mk TyInt (Var tag_v), cases @ default_cases)) in
      mk result_ty (Let (tag_v, tag, sw))
  in
  let emit_switch ~(occ : occ) ~branches ~default : term =
    match occ.ty with
    | TyBool -> emit_bool ~occ ~branches ~default
    | TyInt -> emit_int ~occ ~branches ~default
    | TyFloat -> emit_float ~occ ~branches ~default
    | TyVariant ty_name -> emit_variant ~ty_name ~occ ~branches ~default
    | TyVec _ | TyRecord _ ->
      (match branches with
       | [ (_, body) ] -> body
       | _ -> raise "vec/record match: expected exactly one head" ~loc)
    | TyArrow _ -> raise "cannot match on arrow type" ~loc
    | TySampler -> raise "cannot match on sampler type" ~loc
  in
  let rec go (occs : occ list) (rows : (Frontend.pat list * term) list) : term =
    match Pattern_match.Matrix.classify rows, occs with
    | `Empty, _ -> raise "unreachable: typecheck should have checked exhaustiveness" ~loc
    | `Leaf (pats, body), _ ->
      List.fold2_exn pats occs ~init:body ~f:(fun acc pat occ -> bind_at ~occ pat acc)
    | `Pivot _, [] -> raise "unreachable: pivot exhaustiveness" ~loc
    | `Pivot col_idx, _ :: _ ->
      let occ, rest =
        match pivot_to_front col_idx occs with
        | h :: t -> h, t
        | [] -> raise "unreachable: pivot_to_front preserves size" ~loc
      in
      let rows = List.map rows ~f:(fun (ps, b) -> pivot_to_front col_idx ps, b) in
      let col_ty = occ.ty in
      let heads = column_heads ~loc rows in
      let on_wild_head = bind_at ~occ in
      let branches =
        List.map heads ~f:(fun h ->
          let arity = List.length (head_sub_tys ~col_ty h) in
          let spec =
            Pattern_match.Matrix.specialize
              ~on_wild_head
              ~arity
              ~expand:(pat_args ~col_ty ~h)
              rows
          in
          let sub_pairs = sub_occs_for_head ~occ h in
          let sub_occs = List.map sub_pairs ~f:fst in
          let binds = List.map sub_pairs ~f:(fun (s, b) -> s.name, b) in
          h, go (sub_occs @ rest) spec |> bind_lets ~loc binds)
      in
      let default =
        if signature_complete ~loc ~tenv ~col_ty ~heads
        then None
        else Some (go rest (Pattern_match.Matrix.default ~on_wild_head rows))
      in
      emit_switch ~occ ~branches ~default
  in
  let scrut_var, wrap =
    match scrut.desc with
    | Var v -> v, Fn.id
    | _ ->
      let v = Utils.fresh "_lv_scrut" in
      v, fun (body : term) -> mk body.ty (Let (v, scrut, body))
  in
  let scrut_occ = { name = scrut_var; ty = scrut_pre_ty; loc } in
  let rows = List.map cases ~f:(fun (p, b) -> [ p ], b) in
  wrap (go [ scrut_occ ] rows)
;;

let lower_top (tenv : type_env) (top : Lambda_lift.top) : top =
  let pure desc = ({ desc; ty = lower_ty top.ty; loc = top.loc } : top) in
  match top.desc with
  | TypeDef (name, VariantDecl ctors) ->
    pure (TypeDef (name, RecordDecl (("tag", TyInt) :: slot_layout ctors)))
  | TypeDef (name, RecordDecl fields) ->
    let fields = List.map fields ~f:(Tuple2.map_snd ~f:lower_ty) in
    pure (TypeDef (name, RecordDecl fields))
  | Define { name; recur; args; body; ret_ty } ->
    pure
      (Define
         { name
         ; recur
         ; args = List.map args ~f:(Tuple2.map_snd ~f:lower_ty)
         ; body = lower_term tenv body
         ; ret_ty = lower_ty ret_ty
         })
  | Extern v -> pure (Extern v)
  | Const (name, body) -> pure (Const (name, lower_term tenv body))
;;

let lower (Program tops : Lambda_lift.t) : t Compiler_error.t =
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

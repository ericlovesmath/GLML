open Core
open Compiler_error.Let_syntax
open Anf
open Sexplib.Sexp
open Monomorphize
open Tail_call

(* TODO: Add type for lower_variants to remove variants *)

module Err = Compiler_error.Pass (struct
    let name = "lower_variants"
  end)

type term_desc =
  | Atom of atom
  | Bop of Glsl.binary_op * atom * atom
  | Vec of int * atom list
  | Mat of int * int * atom list
  | Index of atom * int
  | Builtin of Glsl.builtin * atom list
  | App of string * atom list
  | If of atom * anf * anf
  | Record of string * atom list
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
  | Mat (x, y, ts) ->
    List
      (Atom ("mat" ^ Int.to_string x ^ "x" ^ Int.to_string y)
       :: List.map ts ~f:sexp_of_atom)
  | Index (t, i) -> List [ Atom "index"; sexp_of_atom t; Atom (Int.to_string i) ]
  | Builtin (b, ts) ->
    List (Atom (Glsl.string_of_builtin b) :: List.map ts ~f:sexp_of_atom)
  | App (f, args) -> List (Atom f :: List.map args ~f:sexp_of_atom)
  | If (c, t, e) -> List [ Atom "if"; sexp_of_atom c; sexp_of_anf t; sexp_of_anf e ]
  | Record (s, ts) -> List (Atom s :: List.map ts ~f:sexp_of_atom)
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
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyRecord _ -> ty
;;

let find_tag (ctors : (string * ty list) list) (ctor : string) : int Compiler_error.t =
  ctors
  |> List.findi ~f:(fun _ (c, _) -> String.equal c ctor)
  |> Option.map ~f:fst
  |> Err.of_option "unknown ctor" ~d:[%message (ctor : string)]
;;

let find_catchall (cases : (Frontend.pat * _) list) =
  List.find_map cases ~f:(fun (pat, body) ->
    match pat with
    | PatVar v -> Some (v, body)
    | _ -> None)
;;

let bind_opt_var ~loc ~scrut ~ty var_opt (body : anf) : anf =
  match var_opt with
  | None -> body
  | Some v ->
    let bind : term = { desc = Atom scrut; ty; loc } in
    { desc = Let (v, bind, body); ty = body.ty; loc }
;;

let prepend_var_decls
      ~(scrut : Anf.atom)
      ~(loc : Lexer.loc)
      ~(ctors : (string * ty list) list)
      (ctor : string)
      (vars : string list)
      (body : anf)
  : anf Compiler_error.t
  =
  let%bind ctor_tys =
    match List.Assoc.find ctors ~equal:String.equal ctor with
    | Some tys -> Ok tys
    | None -> Err.fail "ctor not in ctors" ~loc ~d:[%message (ctor : string)]
  in
  let fields_with_tys =
    List.mapi ctor_tys ~f:(fun i ty -> [%string "%{ctor}_%{i#Int}"], lower_ty ty)
  in
  match List.zip vars fields_with_tys with
  | Unequal_lengths ->
    Err.fail "vars/ctor_tys length mismatch" ~loc ~d:[%message (ctor : string)]
  | Ok var_field_tys ->
    List.fold_right var_field_tys ~init:(Ok body) ~f:(fun (var, (name, ty)) acc ->
      let%map acc = acc in
      let let_bind = Let (var, { desc = Field (scrut, name); ty; loc }, acc) in
      ({ desc = let_bind; ty = acc.ty; loc } : anf))
;;

let rec map_last_return (k : term -> anf) (anf : anf) : anf =
  match anf.desc with
  | Return term -> k term
  | Let (v, b, t) -> { anf with desc = Let (v, b, map_last_return k t) }
  | While (c, b, t) -> { anf with desc = While (c, b, map_last_return k t) }
  | Set (v, a, t) -> { anf with desc = Set (v, a, map_last_return k t) }
  | Continue -> anf
;;

let rec lower_term (tenv : type_env) (term : Tail_call.term) : term Compiler_error.t =
  let pure desc = Ok ({ desc; ty = lower_ty term.ty; loc = term.loc } : term) in
  match term.desc with
  | Atom a -> pure (Atom a)
  | Bop (op, l, r) -> pure (Bop (op, l, r))
  | Vec (n, ts) -> pure (Vec (n, ts))
  | Mat (x, y, ts) -> pure (Mat (x, y, ts))
  | Index (t, i) -> pure (Index (t, i))
  | Builtin (b, ts) -> pure (Builtin (b, ts))
  | App (f, args) -> pure (App (f, args))
  | Record (s, args) -> pure (Record (s, args))
  | Field (a, f) -> pure (Field (a, f))
  | If (c, t, e) ->
    let%bind t = lower_anf tenv t in
    let%bind e = lower_anf tenv e in
    pure (If (c, t, e))
  | Variant (ty_name, ctor, args) ->
    (match Map.find tenv ty_name with
     | Some (VariantDecl ctors) ->
       let%bind tag, _ =
         ctors
         |> List.findi ~f:(fun _ (c, _) -> String.equal c ctor)
         |> Err.of_option
              "unknown constructor"
              ~d:[%message (ctor : string) (ty_name : string)]
       in
       let%bind flat_atoms =
         let placeholder ty = Ok ({ desc = Temp; ty; loc = term.loc } : atom) in
         ctors
         |> List.concat_map ~f:(fun (c, arg_tys) ->
           if String.equal c ctor
           then List.map args ~f:Compiler_error.return
           else List.map arg_tys ~f:placeholder)
         |> Compiler_error.all
       in
       let tag_atom : atom = { desc = Int tag; ty = TyInt; loc = term.loc } in
       pure (Record (ty_name, tag_atom :: flat_atoms))
     | Some (RecordDecl _) | None ->
       Err.fail "unknown variant type" ~d:[%message (ty_name : string)])
  | Match _ -> Err.fail "match should be handled in lower_anf"

and lower_anf (tenv : type_env) (anf : Tail_call.anf) : anf Compiler_error.t =
  let make desc : anf = { desc; ty = lower_ty anf.ty; loc = anf.loc } in
  let pure desc = Ok (make desc) in
  match anf.desc with
  | Let (v, { desc = Match (scrut, cases); ty; _ }, tail) ->
    let%bind tail = lower_anf tenv tail in
    lower_match tenv scrut cases ty anf.loc (fun t -> make (Let (v, t, tail)))
  | Return { desc = Match (scrut, cases); ty; _ } ->
    lower_match tenv scrut cases ty anf.loc (fun t -> make (Return t))
  | Let (v, term, tail) ->
    let%bind term = lower_term tenv term in
    let%bind tail = lower_anf tenv tail in
    pure (Let (v, term, tail))
  | Return term ->
    let%bind term = lower_term tenv term in
    pure (Return term)
  | While (cond, body, after) ->
    let%bind cond = lower_term tenv cond in
    let%bind body = lower_anf tenv body in
    let%bind after = lower_anf tenv after in
    pure (While (cond, body, after))
  | Set (v, a, tail) ->
    let%bind tail = lower_anf tenv tail in
    pure (Set (v, a, tail))
  | Continue -> pure Continue

and lower_variant_match
      (tenv : type_env)
      (scrut : Anf.atom)
      (cases : (Frontend.pat * Tail_call.anf) list)
      (result_ty : ty)
      (loc : Lexer.loc)
      (k : term -> anf)
  : anf Compiler_error.t
  =
  let result_ty = lower_ty result_ty in
  let%bind ty_name =
    match scrut.ty with
    | TyVariant name -> Ok name
    | _ -> Err.fail "unreachable: scrut is not a variant" ~loc
  in
  let%bind ctors =
    match Map.find tenv ty_name with
    | Some (VariantDecl ctors) -> Ok ctors
    | _ -> Err.fail "unreachable: scrut name is not a variant declration" ~loc
  in
  let lower_case ctor vars body =
    let%bind lowered = lower_anf tenv body in
    prepend_var_decls ~scrut ~loc ~ctors ctor vars lowered
  in
  (* Separate non-wildcard cases from a possible catch-all *)
  let ctor_cases =
    List.filter_map cases ~f:(fun (pat, body) ->
      match pat with
      | PatCtor (ctor, vars) -> Some (ctor, vars, body)
      | _ -> None)
  in
  let catchall_case = find_catchall cases in
  match ctor_cases with
  | [ (ctor, vars, body) ] when Option.is_none catchall_case ->
    let%bind branch = lower_case ctor vars body in
    Ok (map_last_return k branch)
  | _ ->
    let n_ctor = List.length ctor_cases in
    let has_default = Option.is_some catchall_case in
    let%bind switch_cases =
      List.mapi ctor_cases ~f:(fun idx (ctor, vars, body) ->
        let%bind tag = find_tag ctors ctor in
        let%bind branch = lower_case ctor vars body in
        let is_last = idx = n_ctor - 1 && not has_default in
        let label : Glsl.switch_case = if is_last then Default else Case tag in
        Ok (label, branch))
      |> Compiler_error.all
    in
    let%bind default_cases =
      match catchall_case with
      | None -> Ok []
      | Some (v, body) ->
        let%bind body = lower_anf tenv body in
        let body = bind_opt_var ~loc ~scrut ~ty:(TyRecord ty_name) (Some v) body in
        Ok [ Glsl.Default, body ]
    in
    let tag_v = Utils.fresh "_lv_tag" in
    let tag_v_atom : atom = { desc = Var tag_v; ty = TyVariant ty_name; loc } in
    let tag_term : term = { desc = Field (scrut, "tag"); ty = TyInt; loc } in
    let switch_term : term =
      { desc = Switch (tag_v_atom, switch_cases @ default_cases); ty = result_ty; loc }
    in
    Ok ({ desc = Let (tag_v, tag_term, k switch_term); ty = result_ty; loc } : anf)

(** NOTE: GLSL can't match on bools, which is why this is pulled out *)
and lower_bool_match
      (tenv : type_env)
      (scrut : Anf.atom)
      (cases : (Frontend.pat * Tail_call.anf) list)
      (result_ty : ty)
      (loc : Lexer.loc)
      (k : term -> anf)
  : anf Compiler_error.t
  =
  (* TODO: Maybe be can merge the lowering match functions *)
  let find_branch b =
    List.find_map cases ~f:(fun (pat, body) ->
      match pat with
      | PatLitBool lit when Bool.equal lit b -> Some (None, body)
      | PatVar v -> Some (Some v, body)
      | _ -> None)
  in
  let%bind t, t_body = find_branch true |> Err.of_option "no true in bool match" in
  let%bind f, f_body = find_branch false |> Err.of_option "no false in bool match" in
  let%bind t_anf = lower_anf tenv t_body in
  let%bind f_anf = lower_anf tenv f_body in
  let t_anf = bind_opt_var ~loc ~scrut ~ty:TyBool t t_anf in
  let f_anf = bind_opt_var ~loc ~scrut ~ty:TyBool f f_anf in
  let ty = lower_ty result_ty in
  let if_term : term = { desc = If (scrut, t_anf, f_anf); ty; loc } in
  Ok (k if_term)

and lower_int_match
      (tenv : type_env)
      (scrut : Anf.atom)
      (cases : (Frontend.pat * Tail_call.anf) list)
      (result_ty : ty)
      (loc : Lexer.loc)
      (k : term -> anf)
  : anf Compiler_error.t
  =
  let result_ty = lower_ty result_ty in
  let int_cases =
    List.filter_map cases ~f:(fun (pat, body) ->
      match pat with
      | PatLitInt n -> Some (n, body)
      | _ -> None)
  in
  let default_case = find_catchall cases in
  let%bind switch_cases =
    List.map int_cases ~f:(fun (n_val, body) ->
      let%bind lowered = lower_anf tenv body in
      Ok (Glsl.Case n_val, lowered))
    |> Compiler_error.all
  in
  let%bind default_cases =
    match default_case with
    | None -> Ok []
    | Some (v, body) ->
      let%bind lowered = lower_anf tenv body in
      let lowered = bind_opt_var ~loc ~scrut ~ty:TyInt (Some v) lowered in
      Ok [ Glsl.Default, lowered ]
  in
  let switch_term : term =
    { desc = Switch (scrut, switch_cases @ default_cases); ty = result_ty; loc }
  in
  Ok (k switch_term)

(** NOTE: GLSL has no float switch, so this lowers to a nested if-else chain *)
and lower_float_match
      (tenv : type_env)
      (scrut : Anf.atom)
      (cases : (Frontend.pat * Tail_call.anf) list)
      (result_ty : ty)
      (loc : Lexer.loc)
      (k : term -> anf)
  : anf Compiler_error.t
  =
  let ty = lower_ty result_ty in
  let float_cases =
    List.filter_map cases ~f:(fun (pat, body) ->
      match pat with
      | PatLitFloat f -> Some (f, body)
      | _ -> None)
  in
  let default_case = find_catchall cases in
  let%bind lowered_float_cases =
    List.map float_cases ~f:(fun (f_val, body) ->
      let%map lowered = lower_anf tenv body in
      f_val, lowered)
    |> Compiler_error.all
  in
  let%bind default_anf =
    match default_case with
    | None -> Err.fail "float match: missing catch-all"
    | Some (v, body) ->
      let%map lowered = lower_anf tenv body in
      bind_opt_var ~loc ~scrut ~ty:TyFloat (Some v) lowered
  in
  match lowered_float_cases with
  | [] -> Ok default_anf
  | (f, body) :: tl ->
    let%bind inner_else =
      List.fold_right tl ~init:(Ok default_anf) ~f:(fun (f_val, case_body) acc ->
        let%bind acc = acc in
        let cmp_v = Utils.fresh "_lv_cmp" in
        let cmp_v_atom : atom = { desc = Var cmp_v; ty = TyBool; loc } in
        let cmp_term : term =
          let f_val : atom = { desc = Float f_val; ty = TyFloat; loc } in
          { desc = Bop (Glsl.Eq, scrut, f_val); ty = TyBool; loc }
        in
        let if_term : term = { desc = If (cmp_v_atom, case_body, acc); ty; loc } in
        Ok
          ({ desc = Let (cmp_v, cmp_term, { desc = Return if_term; ty; loc }); ty; loc }
           : anf))
    in
    (* TODO: Dedup from above *)
    let cmp_v = Utils.fresh "_lv_cmp" in
    let cmp_v_atom : atom = { desc = Var cmp_v; ty = TyBool; loc } in
    let f : atom = { desc = Float f; ty = TyFloat; loc } in
    let cmp_term : term = { desc = Bop (Glsl.Eq, scrut, f); ty = TyBool; loc } in
    let if_term : term = { desc = If (cmp_v_atom, body, inner_else); ty; loc } in
    Ok ({ desc = Let (cmp_v, cmp_term, k if_term); ty; loc } : anf)

and lower_match
      (tenv : type_env)
      (scrut : Anf.atom)
      (cases : (Frontend.pat * Tail_call.anf) list)
      (result_ty : ty)
      (loc : Lexer.loc)
      (k : term -> anf)
  : anf Compiler_error.t
  =
  (* Determine match kind from first var pattern *)
  let kind =
    List.find_map cases ~f:(fun (pat, _) ->
      match pat with
      | PatVar _ -> None
      | _ -> Some pat)
  in
  match kind with
  | None | Some (PatVar _) ->
    (* [PatVar] never exists, but needed for exhaustiveness, hacky? *)
    (match cases with
     | [] -> Err.fail "empty cases"
     | (pat, body) :: _ ->
       let%map lowered = lower_anf tenv body in
       let lowered =
         match pat with
         | PatVar v ->
           let bind : term = { desc = Atom scrut; ty = lower_ty result_ty; loc } in
           ({ desc = Let (v, bind, lowered); ty = lowered.ty; loc } : anf)
         | _ -> lowered
       in
       map_last_return k lowered)
  | Some (PatCtor _) -> lower_variant_match tenv scrut cases result_ty loc k
  | Some (PatLitBool _) -> lower_bool_match tenv scrut cases result_ty loc k
  | Some (PatLitInt _) -> lower_int_match tenv scrut cases result_ty loc k
  | Some (PatLitFloat _) -> lower_float_match tenv scrut cases result_ty loc k
;;

let lower_top (tenv : type_env) (top : Tail_call.top) : top Compiler_error.t =
  let pure desc = Ok ({ desc; ty = lower_ty top.ty; loc = top.loc } : top) in
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
    let%bind body = lower_anf tenv body in
    pure (Define { name; args; body; ret_ty })
  | Extern v -> pure (Extern v)
  | Const (name, body) ->
    let%bind body = lower_anf tenv body in
    pure (Const (name, body))
;;

let lower (Program tops : Tail_call.t) : t Compiler_error.t =
  let%bind tenv =
    tops
    |> List.filter_map ~f:(fun top ->
      match top.desc with
      | TypeDef (s, decl) -> Some (s, decl)
      | Define _ | Extern _ | Const _ -> None)
    |> String.Map.of_alist_or_error
    |> Err.of_or_error
  in
  let%map tops = Compiler_error.all (List.map tops ~f:(lower_top tenv)) in
  Program tops
;;

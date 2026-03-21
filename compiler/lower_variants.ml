open Core
open Or_error.Let_syntax
open Monomorphize
open Tail_call

(* TODO: Create type excluding variants? *)

type type_env = type_decl String.Map.t

let rec lower_ty (ty : ty) : ty =
  match ty with
  | TyVariant s -> TyRecord s
  | TyArrow (a, b) -> TyArrow (lower_ty a, lower_ty b)
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyRecord _ -> ty
;;

(* TODO: Some fundamental changes might be needed to support
   higher ordered functions in structs/variants *)
let placeholder_atom_for_ty (ty : ty) : Anf.atom Or_error.t =
  match ty with
  | TyFloat -> Ok (Float 0.0)
  | TyInt -> Ok (Int 0)
  | TyBool -> Ok (Bool false)
  | _ -> error_s [%message "lower_variants: cannot create atom placeholder" (ty : ty)]
;;

let find_ctor_info (tenv : type_env) (ctor : string)
  : (int * (string * ty list) list) Or_error.t
  =
  Map.data tenv
  |> List.find_map ~f:(function
    | RecordDecl _ -> None
    | VariantDecl ctors ->
      ctors
      |> List.findi ~f:(fun _ (c, _) -> String.equal c ctor)
      |> Option.map ~f:(fun (i, _) -> i, ctors))
  |> Or_error.of_option
       ~error:(Error.of_lazy_sexp (lazy [%message "lower_variants: unknown ctor" ctor]))
;;

let find_tag (ctors : (string * ty list) list) (ctor : string) : int Or_error.t =
  ctors
  |> List.findi ~f:(fun _ (c, _) -> String.equal c ctor)
  |> Option.map ~f:fst
  |> Or_error.of_option
       ~error:(Error.of_lazy_sexp (lazy [%message "lower_variants: unknown ctor" ctor]))
;;

let prepend_var_decls
      ~(scrut : Anf.atom)
      ~(loc : Lexer.loc)
      ~(ctors : (string * ty list) list)
      (ctor : string)
      (vars : string list)
      (body : anf)
  : anf Or_error.t
  =
  let%bind ctor_tys =
    match List.Assoc.find ctors ~equal:String.equal ctor with
    | Some tys -> Ok tys
    | None -> error_s [%message "lower_variants: ctor not in ctors" ctor]
  in
  let fields_with_tys =
    List.mapi ctor_tys ~f:(fun i ty -> [%string "%{ctor}_%{i#Int}"], lower_ty ty)
  in
  match List.zip vars fields_with_tys with
  | Unequal_lengths ->
    error_s [%message "lower_variants: vars/ctor_tys length mismatch" ctor]
  | Ok var_field_tys ->
    List.fold_right var_field_tys ~init:(Ok body) ~f:(fun (var, (name, ty)) acc ->
      let%map acc = acc in
      let let_bind = Let (var, { desc = Field (scrut, name); ty; loc }, acc) in
      ({ desc = let_bind; ty = acc.ty; loc } : anf))
;;

let rec lower_term (tenv : type_env) (term : term) : term Or_error.t =
  let pure desc = Ok ({ desc; ty = lower_ty term.ty; loc = term.loc } : term) in
  match term.desc with
  | Atom _ | Bop _ | Vec _ | Mat _ | Index _ | Builtin _ | App _ | Record _ | Field _ ->
    pure term.desc
  | If (c, t, e) ->
    let%bind t = lower_anf tenv t in
    let%bind e = lower_anf tenv e in
    pure (If (c, t, e))
  | Variant (ty_name, ctor, args) ->
    (match Map.find tenv ty_name with
     | Some (VariantDecl ctors) ->
       let%bind tag, _ =
         let err =
           lazy [%message "lower_variants: unknown constructor" ctor (ty_name : string)]
         in
         ctors
         |> List.findi ~f:(fun _ (c, _) -> String.equal c ctor)
         |> Or_error.of_option ~error:(Error.of_lazy_sexp err)
       in
       let%bind flat_atoms =
         ctors
         |> List.concat_map ~f:(fun (c, arg_tys) ->
           if String.equal c ctor
           then List.map args ~f:return
           else List.map arg_tys ~f:placeholder_atom_for_ty)
         |> Or_error.all
       in
       pure (Record (ty_name, Anf.Int tag :: flat_atoms))
     | Some (RecordDecl _) | None ->
       error_s [%message "lower_variants: unknown variant type" ty_name])
  | Match _ -> error_s [%message "lower_variants: match should be handled in lower_anf"]
  | Switch _ ->
    error_s [%message "lower_variants: switch should not exist before lowering"]

and lower_anf (tenv : type_env) (anf : anf) : anf Or_error.t =
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
  | Continue -> Ok anf

and lower_match
      (tenv : type_env)
      (scrut : Anf.atom)
      (cases : (string * string list * anf) list)
      (result_ty : ty)
      (loc : Lexer.loc)
      (cont : term -> anf)
  : anf Or_error.t
  =
  let result_ty = lower_ty result_ty in
  let%bind first_ctor, _, _ =
    List.hd cases |> Or_error.of_option ~error:(Error.of_string "lower_variants: hd exn")
  in
  let%bind _, ctors = find_ctor_info tenv first_ctor in
  let lower_case ctor vars body =
    let%bind lowered = lower_anf tenv body in
    prepend_var_decls ~scrut ~loc ~ctors ctor vars lowered
  in
  match cases with
  | [] -> error_s [%message "lower_variants: empty cases"]
  | [ (ctor, vars, body) ] ->
    (* TODO: This is an artifact of our stupid [lower_match] impl,
       have this so it just flattens out the vars like normal *)
    (* Single constructor uses if instead of switch/case *)
    let%bind branch = lower_case ctor vars body in
    Ok (cont { desc = If (Anf.Bool true, branch, branch); ty = result_ty; loc })
  | _ ->
    (* TODO: Have last case be [default] because some GLSL versions require exhaustive *)
    let%bind switch_cases =
      cases
      |> List.map ~f:(fun (ctor, vars, body) ->
        let%bind tag = find_tag ctors ctor in
        let%map branch = lower_case ctor vars body in
        tag, branch)
      |> Or_error.all
    in
    let tag_v = Utils.fresh "_lv_tag" in
    let tag_term : term = { desc = Field (scrut, "tag"); ty = TyInt; loc } in
    let switch_term : term =
      { desc = Switch (Var tag_v, switch_cases); ty = result_ty; loc }
    in
    Ok ({ desc = Let (tag_v, tag_term, cont switch_term); ty = result_ty; loc } : anf)
;;

let lower_top (tenv : type_env) (top : top) : top Or_error.t =
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

let lower (Program tops : t) : t Or_error.t =
  let%bind tenv =
    tops
    |> List.filter_map ~f:(fun top ->
      match top.desc with
      | TypeDef (s, decl) -> Some (s, decl)
      | Define _ | Extern _ | Const _ -> None)
    |> String.Map.of_alist_or_error
  in
  let%map tops = Or_error.all (List.map tops ~f:(lower_top tenv)) in
  Program tops
;;

open Core
open Compiler_error.Let_syntax
open Anf
open Sexplib.Sexp
open Monomorphize
open Tail_call

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
  | Switch of atom * (int * anf) list

and term =
  { desc : term_desc
  ; ty : ty
  ; loc : Lexer.loc
  }

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
    let sexp_of_case (i, body) = List [ Atom (Int.to_string i); sexp_of_anf body ] in
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

(* TODO: Some fundamental changes might be needed to support
   higher ordered functions in structs/variants *)
let placeholder_atom_for_ty (ty : ty) : Anf.atom Compiler_error.t =
  match ty with
  | TyFloat -> Ok (Float 0.0)
  | TyInt -> Ok (Int 0)
  | TyBool -> Ok (Bool false)
  | _ -> Err.fail "cannot create atom placeholder" ~d:[%message (ty : ty)]
;;

let find_ctor_info (tenv : type_env) (ctor : string)
  : (int * (string * ty list) list) Compiler_error.t
  =
  Map.data tenv
  |> List.find_map ~f:(function
    | RecordDecl _ -> None
    | VariantDecl ctors ->
      ctors
      |> List.findi ~f:(fun _ (c, _) -> String.equal c ctor)
      |> Option.map ~f:(fun (i, _) -> i, ctors))
  |> Err.of_option "unknown ctor" ~d:[%message (ctor : string)]
;;

let find_tag (ctors : (string * ty list) list) (ctor : string) : int Compiler_error.t =
  ctors
  |> List.findi ~f:(fun _ (c, _) -> String.equal c ctor)
  |> Option.map ~f:fst
  |> Err.of_option "unknown ctor" ~d:[%message (ctor : string)]
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
         ctors
         |> List.concat_map ~f:(fun (c, arg_tys) ->
           if String.equal c ctor
           then List.map args ~f:Compiler_error.return
           else List.map arg_tys ~f:placeholder_atom_for_ty)
         |> Compiler_error.all
       in
       pure (Record (ty_name, Anf.Int tag :: flat_atoms))
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

and lower_match
      (tenv : type_env)
      (scrut : Anf.atom)
      (cases : (string * string list * Tail_call.anf) list)
      (result_ty : ty)
      (loc : Lexer.loc)
      (k : term -> anf)
  : anf Compiler_error.t
  =
  let result_ty = lower_ty result_ty in
  let%bind first_ctor, _, _ = List.hd cases |> Err.of_option "empty cases" in
  let%bind _, ctors = find_ctor_info tenv first_ctor in
  let lower_case ctor vars body =
    let%bind lowered = lower_anf tenv body in
    prepend_var_decls ~scrut ~loc ~ctors ctor vars lowered
  in
  match cases with
  | [] -> Err.fail "empty cases"
  | [ (ctor, vars, body) ] ->
    let%bind branch = lower_case ctor vars body in
    Ok (map_last_return k branch)
  | _ ->
    let%bind switch_cases =
      cases
      |> List.map ~f:(fun (ctor, vars, body) ->
        let%bind tag = find_tag ctors ctor in
        let%map branch = lower_case ctor vars body in
        tag, branch)
      |> Compiler_error.all
    in
    let tag_v = Utils.fresh "_lv_tag" in
    let tag_term : term = { desc = Field (scrut, "tag"); ty = TyInt; loc } in
    let switch_term : term =
      { desc = Switch (Var tag_v, switch_cases); ty = result_ty; loc }
    in
    Ok ({ desc = Let (tag_v, tag_term, k switch_term); ty = result_ty; loc } : anf)
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

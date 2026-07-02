open Core
open Typecheck
open Type_system

include Compiler_error.Pass (struct
    let name = "erase"
  end)

(** Map [f] over every [ty] *)
let map_tys ~(f : ty -> ty) (Program tops : Typecheck.t) : Typecheck.t =
  let map_type_decl : type_decl -> type_decl = function
    | RecordDecl (ps, fields) -> RecordDecl (ps, List.map fields ~f:(fun (n, t) -> n, f t))
    | VariantDecl (ps, ctors) ->
      VariantDecl (ps, List.map ctors ~f:(fun (n, ts) -> n, List.map ts ~f))
  in
  let map_top (top : top) : top =
    let desc =
      match top.desc with
      | Define (r, v, bind) -> Define (r, v, map_term_tys ~f bind)
      | Extern _ -> top.desc
      | TypeDef (n, decl) -> TypeDef (n, map_type_decl decl)
    in
    let scheme_constrs = List.map top.scheme_constrs ~f:(map_constr_tys ~f) in
    { top with desc; ty = f top.ty; scheme_constrs }
  in
  Program (List.map tops ~f:map_top)
;;

let erase ({ program; reveal } : Typecheck.elaborated) : Typecheck.t Compiler_error.t =
  try_with (fun () ->
    if Map.is_empty reveal
    then program
    else (
      let rec erase_ty (ty : ty) : ty =
        match ty with
        | TyAbstract g ->
          (match Map.find reveal g with
           | Some repr -> erase_ty repr
           | None -> raise "unrevealed abstract type stamp" ~d:[%message (g : string)])
        | ty -> map_ty_children erase_ty ty
      in
      map_tys ~f:erase_ty program))
;;

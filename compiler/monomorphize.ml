open Core
open Stlc
open Or_error.Let_syntax

let rec has_tyvars (ty : ty) : bool =
  match ty with
  | TyVar _ -> true
  | TyFloat | TyInt | TyBool | TyVec _ | TyMat _ | TyRecord _ -> false
  | TyArrow (a, b) -> has_tyvars a || has_tyvars b
;;

let rec subst ~(poly : ty) ~(concrete : ty) : (string * ty) list =
  match poly, concrete with
  | TyVar v, _ -> [ v, concrete ]
  | TyArrow (l, r), TyArrow (l', r') ->
    subst ~poly:l ~concrete:l' @ subst ~poly:r ~concrete:r'
  | _, _ -> []
;;

(** String suffix to add to generated functions *)
let rec mangle_ty (ty : ty) : string =
  match ty with
  | TyFloat -> "float"
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyVec n -> "vec" ^ Int.to_string n
  | TyMat (x, y) -> "mat" ^ Int.to_string x ^ "x" ^ Int.to_string y
  | TyRecord s -> s
  | TyVar v -> "tv" ^ v
  | TyArrow (a, b) -> mangle_ty a ^ "_to_" ^ mangle_ty b
;;

(** Collect all concrete types a variable is used at in a term *)
let collect_var_usages (name : string) (t : Typecheck.term) : ty list =
  let rec walk (acc : ty list) (t : Typecheck.term) : ty list =
    let acc =
      match t.desc with
      | Var v when String.equal v name -> t.ty :: acc
      | _ -> acc
    in
    match t.desc with
    | Var _ | Float _ | Int _ | Bool _ -> acc
    | Vec (_, ts) | Mat (_, _, ts) | Builtin (_, ts) | Record (_, ts) ->
      List.fold ts ~init:acc ~f:walk
    | Lam (_, _, body) -> walk acc body
    | App (f, x) -> walk (walk acc f) x
    | Let (_, _, bind, body) -> walk (walk acc bind) body
    | If (c, t, e) -> walk (walk (walk acc c) t) e
    | Bop (_, l, r) -> walk (walk acc l) r
    | Index (t, _) | Field (t, _) -> walk acc t
  in
  let usages = walk [] t in
  (* Deduplicate by structural equality *)
  List.dedup_and_sort usages ~compare:(fun a b ->
    if equal_ty a b then 0 else Sexp.compare (sexp_of_ty a) (sexp_of_ty b))
;;

(** Rename variable references from old_name to new_name *)
let rec rename_var (src : string) (dst : string) (t : Typecheck.term) : Typecheck.term =
  let rename = rename_var src dst in
  let desc : Typecheck.term_desc =
    match t.desc with
    | Var v when String.equal v src -> Var dst
    | Var _ | Float _ | Int _ | Bool _ -> t.desc
    | Vec (n, ts) -> Vec (n, List.map ts ~f:rename)
    | Mat (n, m, ts) -> Mat (n, m, List.map ts ~f:rename)
    | Lam (v, ty, body) -> if String.equal v src then t.desc else Lam (v, ty, rename body)
    | App (f, x) -> App (rename f, rename x)
    | Let (recur, v, bind, body) ->
      let bind = rename bind in
      let body = if String.equal v src then body else rename body in
      Let (recur, v, bind, body)
    | If (c, t, e) -> If (rename c, rename t, rename e)
    | Bop (op, l, r) -> Bop (op, rename l, rename r)
    | Index (t, i) -> Index (rename t, i)
    | Builtin (b, ts) -> Builtin (b, List.map ts ~f:rename)
    | Record (s, ts) -> Record (s, List.map ts ~f:rename)
    | Field (t, f) -> Field (rename t, f)
  in
  { t with desc }
;;

type spec =
  { poly_type : ty
  ; poly_bind : Typecheck.term
  ; poly_recur : recur
  ; poly_loc : Lexer.loc
  ; specs : (ty * string) list (** concrete type to specialized name *)
  }

(* TODO: Use a state monad or something? *)
(* TODO: [Use List.concat_map style to avoid this?] *)
type state =
  { poly_env : spec String.Map.t
  ; pending_defs : Typecheck.top list
  }

let fold_map st xs ~f =
  let st, xs_rev =
    List.fold xs ~init:(st, []) ~f:(fun (st, acc) x ->
      let st, x' = f st x in
      st, x' :: acc)
  in
  st, List.rev xs_rev
;;

(** Returns existing or new polymorphic definition *)
let rec get_or_create_spec (st : state) (name : string) (entry : spec) (concrete_ty : ty)
  : state * string
  =
  match List.find entry.specs ~f:(fun (ty, _) -> equal_ty ty concrete_ty) with
  | Some (_, spec_name) -> st, spec_name
  | None ->
    let spec_name = Utils.fresh (name ^ "_" ^ mangle_ty concrete_ty) in
    let entry = { entry with specs = (concrete_ty, spec_name) :: entry.specs } in
    let st = { st with poly_env = Map.set st.poly_env ~key:name ~data:entry } in
    let sub = subst ~poly:entry.poly_type ~concrete:concrete_ty in
    let body = Typecheck.subst_term sub entry.poly_bind in
    (* For recursive functions, rename self-references *)
    let body =
      match entry.poly_recur with
      | Rec _ -> rename_var name spec_name body
      | Nonrec -> body
    in
    (* Rewrite any poly references in the specialized body *)
    let st, body = rewrite_term st body in
    let recur =
      match entry.poly_recur with
      | Rec (n, _) -> Rec (n, Some concrete_ty)
      | Nonrec -> Nonrec
    in
    let top : Typecheck.top =
      { desc = Define (recur, spec_name, body); ty = concrete_ty; loc = entry.poly_loc }
    in
    { st with pending_defs = top :: st.pending_defs }, spec_name

(** Replaces references to polymorphic functions with specialized ones *)
and rewrite_term (st : state) (t : Typecheck.term) : state * Typecheck.term =
  let (desc : Typecheck.term_desc), st =
    match t.desc with
    | Var v ->
      (match Map.find st.poly_env v with
       | Some entry when not (has_tyvars t.ty) ->
         let st, spec_name = get_or_create_spec st v entry t.ty in
         Var spec_name, st
       | _ -> t.desc, st)
    | Float _ | Int _ | Bool _ -> t.desc, st
    | Vec (n, ts) ->
      let st, ts = fold_map st ts ~f:rewrite_term in
      Vec (n, ts), st
    | Mat (n, m, ts) ->
      let st, ts = fold_map st ts ~f:rewrite_term in
      Mat (n, m, ts), st
    | Lam (v, ty, body) ->
      let st, body = rewrite_term st body in
      Lam (v, ty, body), st
    | App (f, x) ->
      let st, f = rewrite_term st f in
      let st, x = rewrite_term st x in
      App (f, x), st
    | Let (recur, v, bind, body) when has_tyvars bind.ty ->
      (* Specialization for inner polymorphic lets *)
      let usages = collect_var_usages v body in
      if List.is_empty usages
      then (
        (* Dead code, just rewrite body *)
        let st, body = rewrite_term st body in
        body.desc, st)
      else (
        let st, specs =
          fold_map st usages ~f:(fun st concrete_ty ->
            let sub = subst ~poly:bind.ty ~concrete:concrete_ty in
            let spec_bind = Typecheck.subst_term sub bind in
            let spec_name = Utils.fresh (v ^ "_" ^ mangle_ty concrete_ty) in
            let st, spec_bind =
              match recur with
              | Rec _ ->
                let st, rewritten = rewrite_term st spec_bind in
                st, (spec_name, rename_var v spec_name rewritten, concrete_ty)
              | Nonrec ->
                let st, rewritten = rewrite_term st spec_bind in
                st, (spec_name, rewritten, concrete_ty)
            in
            st, spec_bind)
        in
        (* Rewrite body replacing [Var v] with appropriate spec name *)
        let body' =
          List.fold specs ~init:body ~f:(fun b (spec_name, _, concrete_ty) ->
            rewrite_var_at_type v spec_name concrete_ty b)
        in
        let st, body' = rewrite_term st body' in
        (* Wrap in nested lets *)
        ( (List.fold_right
             specs
             ~init:body'
             ~f:(fun (spec_name, spec_bind, concrete_ty) acc ->
               let recur =
                 match recur with
                 | Rec (n, _) -> Rec (n, Some concrete_ty)
                 | Nonrec -> Nonrec
               in
               ({ desc = Let (recur, spec_name, spec_bind, acc)
                ; ty = acc.ty
                ; loc = t.loc
                }
                : Typecheck.term)))
            .desc
        , st ))
    | Let (recur, v, bind, body) ->
      let st, bind = rewrite_term st bind in
      let st, body = rewrite_term st body in
      Let (recur, v, bind, body), st
    | If (c, t, e) ->
      let st, c = rewrite_term st c in
      let st, t = rewrite_term st t in
      let st, e = rewrite_term st e in
      If (c, t, e), st
    | Bop (op, l, r) ->
      let st, l = rewrite_term st l in
      let st, r = rewrite_term st r in
      Bop (op, l, r), st
    | Index (t, i) ->
      let st, t = rewrite_term st t in
      Index (t, i), st
    | Builtin (b, ts) ->
      let st, ts = fold_map st ts ~f:rewrite_term in
      Builtin (b, ts), st
    | Record (s, ts) ->
      let st, ts = fold_map st ts ~f:rewrite_term in
      Record (s, ts), st
    | Field (t, f) ->
      let st, t = rewrite_term st t in
      Field (t, f), st
  in
  st, { t with desc }

(** Replace references to [name] that have a specific type with [new_name] *)
and rewrite_var_at_type
      (name : string)
      (new_name : string)
      (target_ty : ty)
      (t : Typecheck.term)
  : Typecheck.term
  =
  let rewrite = rewrite_var_at_type name new_name target_ty in
  let desc : Typecheck.term_desc =
    match t.desc with
    | Var v when String.equal v name && equal_ty t.ty target_ty -> Var new_name
    | Var _ | Float _ | Int _ | Bool _ -> t.desc
    | Vec (n, ts) -> Vec (n, List.map ts ~f:rewrite)
    | Mat (n, m, ts) -> Mat (n, m, List.map ts ~f:rewrite)
    | Lam (v, ty, body) ->
      if String.equal v name then t.desc else Lam (v, ty, rewrite body)
    | App (f, x) -> App (rewrite f, rewrite x)
    | Let (recur, v, bind, body) ->
      let bind = rewrite bind in
      let body = if String.equal v name then body else rewrite body in
      Let (recur, v, bind, body)
    | If (c, t, e) -> If (rewrite c, rewrite t, rewrite e)
    | Bop (op, l, r) -> Bop (op, rewrite l, rewrite r)
    | Index (t, i) -> Index (rewrite t, i)
    | Builtin (b, ts) -> Builtin (b, List.map ts ~f:rewrite)
    | Record (s, ts) -> Record (s, List.map ts ~f:rewrite)
    | Field (t, f) -> Field (rewrite t, f)
  in
  { t with desc }
;;

let monomorphize (Program tops : Typecheck.t) : Typecheck.t Or_error.t =
  let st = { poly_env = String.Map.empty; pending_defs = [] } in
  let%map _, tops =
    List.fold_result tops ~init:(st, []) ~f:(fun (st, acc) top ->
      match top.desc with
      (* Polymorphic case *)
      | Define (recur, v, bind) when has_tyvars top.ty ->
        let entry =
          { poly_type = top.ty
          ; poly_bind = bind
          ; poly_recur = recur
          ; poly_loc = top.loc
          ; specs = []
          }
        in
        let poly_env = Map.set st.poly_env ~key:v ~data:entry in
        Ok ({ st with poly_env }, acc)
        (* Monomorphic case *)
      | Define (recur, v, bind) ->
        let st = { st with pending_defs = [] } in
        let st, bind = rewrite_term st bind in
        let pending = List.rev st.pending_defs in
        let top = { top with desc = Define (recur, v, bind) } in
        Ok (st, top :: (pending @ acc))
      | Extern _ | RecordDef _ -> Ok (st, top :: acc))
  in
  (* TODO: Do something to validate no tyvars remain? *)
  Typecheck.Program (List.rev tops)
;;

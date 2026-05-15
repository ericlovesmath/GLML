open Core
open Frontend
open Type_system

(* A head identifies a row's first-column pattern up to "matches the same
   values." Sub-types are embedded so a head cannot be paired with an
   incompatible column type. Identifying fields use structural equality; the
   embedded type info is [@equal.ignore] so dedup uses only the key. *)
type head =
  | HBool of bool
  | HInt of int
  | HFloat of float
  | HCtor of string * (ty list[@equal.ignore])
  | HBracket of (ty list[@equal.ignore])
  | HRecord of ((string * ty) list[@equal.ignore])
[@@deriving equal]

let head_sub_tys = function
  | HBool _ | HInt _ | HFloat _ -> []
  | HCtor (_, ts) | HBracket ts -> ts
  | HRecord fs -> List.map fs ~f:snd
;;

(** The complete head signature for a [col_ty], or [None] if the type's value
    domain is infinite (int, float). *)
let signature_heads : ty -> head list option = function
  | TyBool -> [ HBool true; HBool false ] |> Some
  | TyVariant (_, ctors) -> List.map ctors ~f:(fun (c, ts) -> HCtor (c, ts)) |> Some
  | TyVec (n, elem) -> [ HBracket (List.init n ~f:(Fn.const elem)) ] |> Some
  | TyRecord (_, fs) -> [ HRecord fs ] |> Some
  | TyInt | TyFloat | TyArrow _ | TyVar _ -> None
;;

(** Head of a non-wild pattern at [col_ty]. Returns [None] for wild/var. *)
let head_of_pat ~(col_ty : ty) : pat -> head option = function
  | PatWildcard | PatVar _ -> None
  | PatLitBool b -> Some (HBool b)
  | PatLitInt n -> Some (HInt n)
  | PatLitFloat f -> Some (HFloat f)
  | PatCtor (c, _) ->
    let arg_tys =
      match col_ty with
      | TyVariant (_, cs) ->
        List.Assoc.find cs ~equal:String.equal c |> Option.value ~default:[]
      | _ -> []
    in
    Some (HCtor (c, arg_tys))
  | PatBracket _ ->
    let sub_tys =
      match col_ty with
      | TyVec (n, elem) -> List.init n ~f:(Fn.const elem)
      | _ -> []
    in
    Some (HBracket sub_tys)
  | PatRecord _ ->
    let fs =
      match col_ty with
      | TyRecord (_, fs) -> fs
      | _ -> []
    in
    Some (HRecord fs)
;;

(** Sub-patterns of [pat] under [h]: [Some args] if [pat]'s head equals [h]
    (wildcards expanded to head arity), [None] if [pat] would be filtered out
    when specializing on [h]. *)
let pat_args ~(h : head) : pat -> pat list option =
  let wilds () = List.map (head_sub_tys h) ~f:(Fn.const PatWildcard) in
  function
  | PatWildcard | PatVar _ -> Some (wilds ())
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
            |> Option.value ~default:PatWildcard))
     | _ -> None)
;;

(** A concrete literal counter-example for a wildcard column in infinite domain *)
let fresh_lit_witness ~(col_ty : ty) ~(present : head list) : pat =
  match col_ty with
  | TyInt ->
    let used =
      List.filter_map present ~f:(function
        | HInt n -> Some n
        | _ -> None)
      |> Int.Set.of_list
    in
    let rec pick n = if Set.mem used n then pick (n + 1) else n in
    PatLitInt (pick 0)
  | TyFloat ->
    let used =
      List.filter_map present ~f:(function
        | HFloat f -> Some f
        | _ -> None)
    in
    let rec pick f = if List.mem used f ~equal:Float.equal then pick (f +. 1.0) else f in
    PatLitFloat (pick 0.0)
  | _ -> PatWildcard
;;

let rebuild_witness (h : head) (subs : pat list) : pat =
  match h with
  | HBool b -> PatLitBool b
  | HInt n -> PatLitInt n
  | HFloat f -> PatLitFloat f
  | HCtor (c, _) -> PatCtor (c, subs)
  | HBracket _ -> PatBracket subs
  | HRecord fs -> PatRecord (List.map2_exn fs subs ~f:(fun (n, _) p -> n, p), false)
;;

module Matrix = struct
  type 'a row = pat list * 'a

  let is_wild : pat -> bool = function
    | PatWildcard | PatVar _ -> true
    | _ -> false
  ;;

  let classify (rows : 'a row list) : [ `Empty | `Leaf of 'a row | `Pivot of int ] =
    match rows with
    | [] -> `Empty
    | ((pats, _) as row) :: _ ->
      (match List.findi pats ~f:(fun _ p -> not (is_wild p)) with
       | None -> `Leaf row
       | Some (i, _) -> `Pivot i)
  ;;

  let specialize
        ~(on_wild_head : pat -> 'a -> 'a)
        ~(expand : pat -> pat list option)
        ~(arity : int)
        (rows : 'a row list)
    : 'a row list
    =
    let wilds = lazy (List.init arity ~f:(Fn.const PatWildcard)) in
    List.filter_map rows ~f:(fun (pats, body) ->
      match pats with
      | [] -> None
      | p :: rest when is_wild p -> Some (Lazy.force wilds @ rest, on_wild_head p body)
      | p :: rest -> expand p |> Option.map ~f:(fun subs -> subs @ rest, body))
  ;;

  let default ~(on_wild_head : pat -> 'a -> 'a) (rows : 'a row list) : 'a row list =
    List.filter_map rows ~f:(fun (pats, body) ->
      match pats with
      | p :: rest when is_wild p -> Some (rest, on_wild_head p body)
      | _ -> None)
  ;;
end

(** Heads observed in column 0 of the matrix, deduplicated. *)
let column_heads ~(col_ty : ty) (rows : 'a Matrix.row list) : head list =
  List.filter_map rows ~f:(function
    | p :: _, _ -> head_of_pat ~col_ty p
    | [], _ -> None)
  |> List.fold ~init:[] ~f:(fun acc h ->
    if List.mem acc h ~equal:equal_head then acc else h :: acc)
  |> List.rev
;;

(** U_rec: [Some witness] iff [row] matches a value no matrix row matches. *)
let rec useful_rec ~col_tys ~(matrix : unit Matrix.row list) ~(row : pat list)
  : pat list option
  =
  match col_tys, row with
  | [], _ -> if List.is_empty matrix then Some [] else None
  | _ :: _, [] -> None
  | col_ty :: rest_tys, q :: rest_row ->
    let try_head h =
      let sub_tys = head_sub_tys h in
      let n_subs = List.length sub_tys in
      let q_subs =
        pat_args ~h q
        |> Option.value ~default:(List.init n_subs ~f:(Fn.const PatWildcard))
      in
      let matrix' =
        Matrix.specialize
          ~on_wild_head:(fun _ b -> b)
          ~expand:(pat_args ~h)
          ~arity:n_subs
          matrix
      in
      useful_rec ~col_tys:(sub_tys @ rest_tys) ~matrix:matrix' ~row:(q_subs @ rest_row)
      |> Option.map ~f:(fun w ->
        rebuild_witness h (List.take w n_subs) :: List.drop w n_subs)
    in
    (match head_of_pat ~col_ty q with
     | Some h -> try_head h
     | None ->
       let present = column_heads ~col_ty matrix in
       let missing_from = function
         | None -> None
         | Some all ->
           List.find all ~f:(fun h -> not (List.mem present h ~equal:equal_head))
       in
       (match signature_heads col_ty, missing_from (signature_heads col_ty) with
        | Some all, None -> List.find_map all ~f:try_head
        | maybe_all, _ ->
          let matrix' = Matrix.default ~on_wild_head:(fun _ b -> b) matrix in
          useful_rec ~col_tys:rest_tys ~matrix:matrix' ~row:rest_row
          |> Option.map ~f:(fun tail ->
            let head_witness =
              match missing_from maybe_all with
              | Some h ->
                rebuild_witness h (List.map (head_sub_tys h) ~f:(Fn.const PatWildcard))
              | None -> fresh_lit_witness ~col_ty ~present
            in
            head_witness :: tail)))
;;

let useful ~col_tys ~matrix ~row : pat option =
  let matrix = List.map matrix ~f:(fun pats -> pats, ()) in
  useful_rec ~col_tys ~matrix ~row |> Option.bind ~f:List.hd
;;

let is_exhaustive ~scrutinee_ty (pats : pat list) : pat option =
  let matrix = List.map pats ~f:List.return in
  useful ~col_tys:[ scrutinee_ty ] ~matrix ~row:[ PatWildcard ]
;;

let is_redundant ~scrutinee_ty (pats : pat list) : int option =
  let rec loop i ~prefix_rev = function
    | [] -> None
    | p :: rest ->
      let matrix = List.rev_map prefix_rev ~f:List.return in
      (match useful ~col_tys:[ scrutinee_ty ] ~matrix ~row:[ p ] with
       | None -> Some i
       | Some _ -> loop (i + 1) ~prefix_rev:(p :: prefix_rev) rest)
  in
  loop 0 ~prefix_rev:[] pats
;;

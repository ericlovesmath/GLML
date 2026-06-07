open Core
open Glsl

module Err = Compiler_error.Pass (struct
    let name = "patch_main"
  end)

(* Rewrite every [return e] in the body into a [fragColor = e] *)
let rec rewrite_returns (ss : stmt list) : stmt list =
  List.concat_map ss ~f:(function
    | Return (Some e) -> [ Set (Var "fragColor", e); Return None ]
    | Return None -> [ Return None ]
    | IfStmt (c, t, e) -> [ IfStmt (c, rewrite_stmt t, Option.map e ~f:rewrite_stmt) ]
    | SwitchStmt (s, cases) ->
      [ SwitchStmt (s, List.map cases ~f:(fun (l, body) -> l, rewrite_returns body)) ]
    | Block ss -> [ Block (rewrite_returns ss) ]
    | For (i, c, u, b) -> [ For (i, c, u, rewrite_stmt b) ]
    | WhileStmt (c, b) -> [ WhileStmt (c, rewrite_stmt b) ]
    | other -> [ other ])

and rewrite_stmt (s : stmt) : stmt =
  match rewrite_returns [ s ] with
  | [ one ] -> one
  | many -> Block many
;;

(* Drop a redundant trailing [return;] *)
let drop_trailing_return (ss : stmt list) : stmt list =
  match List.rev ss with
  | Return None :: rest -> List.rev rest
  | _ -> ss
;;

let patch (Program funcs : t) : t Compiler_error.t =
  let open Compiler_error.Let_syntax in
  let main_count =
    List.count funcs ~f:(function
      | Function { name = "main"; _ } -> true
      | _ -> false)
  in
  if main_count <> 1
  then Err.fail "expected exactly one main function"
  else (
    let%map funcs =
      List.map funcs ~f:(function
        | Function
            ({ name = "main"
             ; desc = _
             ; params = [ (TyVec 2, coord) ]
             ; ret_type = TyVec 4
             ; body
             } as func) ->
          let coord_decl =
            Decl (None, TyVec 2, coord, Some (Swizzle (Var "gl_FragCoord", "xy")))
          in
          let body = coord_decl :: drop_trailing_return (rewrite_returns body) in
          Ok (Function { func with params = []; ret_type = TyVoid; body })
        | Function { name = "main"; _ } as t ->
          Err.fail "unexpected type of main" ~d:[%message (t : decl)]
        | decl -> Ok decl)
      |> Compiler_error.all
    in
    Program (Global (Out, TyVec 4, "fragColor", None) :: funcs))
;;

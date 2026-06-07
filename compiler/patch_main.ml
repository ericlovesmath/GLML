open Core
open Glsl

module Err = Compiler_error.Pass (struct
    let name = "patch_main"
  end)

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
    let main_ret =
      List.find_map funcs ~f:(function
        | Function { name = "main"; params = [ (TyVec 2, _) ]; ret_type; _ } ->
          Some ret_type
        | _ -> None)
      |> Option.value ~default:(TyVec 3)
    in
    let%map funcs =
      List.map funcs ~f:(function
        | Function
            ({ name = "main"
             ; desc = _
             ; params = [ (TyVec 2, _) ]
             ; ret_type = TyVec 3 | TyVec 4
             ; body = _
             } as func) -> Ok (Function { func with name = "main_pure" })
        | Function { name = "main"; _ } as t ->
          Err.fail "unexpected type of main" ~d:[%message (t : decl)]
        | decl -> Ok decl)
      |> Compiler_error.all
    in
    let patched_main =
      let call = App ("main_pure", [ Swizzle (Var "gl_FragCoord", "xy") ]) in
      let body =
        match main_ret with
        | TyVec 4 -> [ Set (Var "fragColor", call) ]
        | _ ->
          (* TODO: Get rid of this Vec3 branch and enforce Vec4 *)
          [ Decl (None, TyVec 3, "color", Some call)
          ; Set
              (Var "fragColor", App ("vec4", [ Swizzle (Var "color", "xyz"); Float 1.0 ]))
          ]
      in
      Function { name = "main"; desc = None; params = []; ret_type = TyVoid; body }
    in
    Program ([ Global (Out, TyVec 4, "fragColor", None) ] @ funcs @ [ patched_main ]))
;;

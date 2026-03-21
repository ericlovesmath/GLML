open Core
open Glsl

let patch (Program funcs : t) : t Or_error.t =
  let open Or_error.Let_syntax in
  let main_count =
    List.count funcs ~f:(function
      | Function { name = "main"; _ } -> true
      | _ -> false)
  in
  if main_count <> 1
  then Or_error.error_string "patch_main: expected exactly one main function"
  else (
    let%map funcs =
      List.map funcs ~f:(function
        | Function
            ({ name = "main"
             ; desc = _
             ; params = [ (TyVec 2, _) ]
             ; ret_type = TyVec 3
             ; body = _
             } as func) -> Ok (Function { func with name = "main_pure" })
        | Function { name = "main"; _ } as t ->
          error_s [%message "patch_main: unexpected type of main" (t : decl)]
        | decl -> Ok decl)
      |> Or_error.all
    in
    let patched_main =
      let body =
        [ Decl
            ( None
            , TyVec 3
            , "color"
            , App ("main_pure", [ Swizzle (Var "gl_FragCoord", "xy") ]) )
        ; Set
            ( Var "fragColor"
            , App
                ( "clamp"
                , [ App ("vec4", [ Swizzle (Var "color", "xyz"); Float 1.0 ])
                  ; Float 0.0
                  ; Float 1.0
                  ] ) )
        ]
      in
      Function { name = "main"; desc = None; params = []; ret_type = TyVoid; body }
    in
    Program ([ Global (Out, TyVec 4, "fragColor", None) ] @ funcs @ [ patched_main ]))
;;

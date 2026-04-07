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
          Err.fail "unexpected type of main" ~d:[%message (t : decl)]
        | decl -> Ok decl)
      |> Compiler_error.all
    in
    let patched_main =
      let body =
        [ Decl
            ( None
            , TyVec 3
            , "color"
            , Some (App ("main_pure", [ Swizzle (Var "gl_FragCoord", "xy") ])) )
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

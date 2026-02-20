open Glml_compiler

(* module%bench [@name "Test Group"] _ = struct end *)

let%bench "benchmark 1" =
  compile_stlc "(let x = 2.0 in (vec3 (+ (* 12.0 x) 10.0) 0.0 0.0))"
;;

let%bench "benchmark 2" =
  compile_stlc
    {|
    ((extern float n)
     (let f = (fun x : float -> (+ x n)))
     (let main = (fun u : unit -> (vec3 (f 10.0) 0.0 0.0))))
    |}
;;

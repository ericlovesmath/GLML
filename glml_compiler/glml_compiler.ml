open Core

(** Passes in compiler available to be dumped, using GADT witnesses to have
    generic [sexp_of_pass] function with [trace] *)
module Passes = struct
  type _ pass =
    | Stlc : Stlc.t pass
    | Uniquify : Stlc.t pass
    | Typecheck : Typecheck.t pass
    | Anf : Anf.t pass
    | Translate : Glsl.t pass

  let sexp_of_pass : type a. a pass -> a -> Sexp.t = function
    | Stlc -> Stlc.sexp_of_t
    | Uniquify -> Stlc.sexp_of_t
    | Typecheck -> Typecheck.sexp_of_t
    | Anf -> Anf.sexp_of_t
    | Translate -> Glsl.sexp_of_t
  ;;

  module T = struct
    type t =
      | Stlc
      | Uniquify
      | Typecheck
      | Anf
      | Translate
    [@@deriving compare, sexp, enumerate, string ~capitalize:"lower sentence case"]
  end

  include T
  include Comparable.Make (T)

  let of_pass : type a. a pass -> t = function
    | Stlc -> Stlc
    | Uniquify -> Uniquify
    | Typecheck -> Typecheck
    | Anf -> Anf
    | Translate -> Translate
  ;;
end

(* TODO: remove this? *)
let compile_source src = Glsl.to_shader (Glsl.of_string src)

let compile_stlc ?(dump : (Sexp.t -> unit) Passes.Map.t = Passes.Map.empty) (s : string)
  : string Or_error.t
  =
  let trace : type a. a Passes.pass -> a -> unit =
    fun pass value ->
    Passes.of_pass pass
    |> Map.find dump
    |> Option.iter ~f:(fun f -> f (Passes.sexp_of_pass pass value))
  in
  let open Or_error.Let_syntax in
  Utils.reset ();
  let%bind t = Stlc.of_string s in
  trace Stlc t;
  let t = Uniquify.uniquify t in
  trace Uniquify t;
  let%bind t = Typecheck.typecheck t in
  trace Typecheck t;
  let t = Anf.to_anf t in
  trace Anf t;
  let glsl = Translate.translate t in
  trace Translate glsl;
  return (Glsl.to_shader glsl)
;;

let%expect_test "simple tests for compile_stlc" =
  let test s =
    match compile_stlc s with
    | Error err -> print_s (Error.sexp_of_t err)
    | Ok glsl -> print_endline glsl
  in
  let wrap_main s = [%string "((let main = (fun u : float -> %{s})))"] in
  test (wrap_main "(let x = 2.0 in (+ (* 12.0 x) 10.0))");
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec3 fragColor;
    void main() {
        float x_1 = 2.;
        float anf_2 = (12. * x_1);
        fragColor = (anf_2 + 10.);
    }
    |}];
  test (wrap_main "(if (&& #t #f) (let x = 2.0 in (* x 1.0)) 2.0)");
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec3 fragColor;
    void main() {
        bool anf_2 = (true && false);
        if (anf_2) {
            float x_1 = 2.;
            return (x_1 * 1.);
        } else {
            return 2.;
        }
    }
    |}];
  test (wrap_main "(let x = (if #f 0 1) in (* x 2))");
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec3 fragColor;
    void main() {
        int x_1 = 0;
        if (false) {
            x_1 = 0;
        } else {
            x_1 = 1;
        }
        fragColor = (x_1 * 2);
    }
    |}];
  test
    {|
    ((let f = (fun x : float -> (+ x 1.0)))
     (let main = (fun u : float -> (vec3 (f 10.0) 0.0 0.0))))
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec3 fragColor;
    float f_1(float x_0) {
        return (x_0 + 1.);
    }
    void main() {
        float anf_3 = f_1(10.);
        fragColor = vec3(anf_3, 0., 0.);
    }
    |}];
  test
    {|
    ((extern float n)
     (let f = (fun x : float -> (+ x n)))
     (let main = (fun u : float -> (vec3 (f 10.0) 0.0 0.0))))
    |};
  [%expect
    {|
    #version 300 es
    precision highp float;
    out vec3 fragColor;
    uniform float n;
    float f_1(float x_0) {
        return (x_0 + n);
    }
    void main() {
        float anf_3 = f_1(10.);
        fragColor = vec3(anf_3, 0., 0.);
    }
    |}];
;;

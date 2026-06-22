open Core
open Glml_compiler

let examples_dir = "../examples"

let report ~name source =
  let len ~optimize source =
    String.length (Compiler_error.ok_exn (compile ~optimize source))
  in
  let noopt = len ~optimize:false source in
  let opt = len ~optimize:true source in
  let diff = Float.((of_int opt - of_int noopt) / of_int noopt * 100.) in
  printf "%-36s %8d %8d %+7.1f%%\n" name noopt opt diff
;;

let%expect_test "optimizer shrinks output" =
  printf "%-36s %8s %8s %8s\n" "input" "noopt" "opt" "diff";
  let files = Stdlib.Sys.readdir examples_dir in
  Array.sort files ~compare:String.compare;
  Array.iter files ~f:(fun name ->
    report ~name (In_channel.read_all (Filename.concat examples_dir name)));
  [%expect
    {|
    input                                   noopt      opt     diff
    2d_sdf_variants.glml                    16337     2727   -83.3%
    beaver.glml                             19384    14128   -27.1%
    bezier.buffer_a.glml                     3302     2866   -13.2%
    bezier.image.glml                        8428     5071   -39.8%
    game_of_life.buffer_a.glml               2293     2213    -3.5%
    game_of_life.image.glml                   234      234    +0.0%
    mandelbrot.glml                          3835     3055   -20.3%
    materials.glml                          13772     7481   -45.7%
    menger_sponge.glml                       9259     6925   -25.2%
    planet.glml                             11930    10817    -9.3%
    raymarch.glml                            7042     5939   -15.7%
    reaction_diffusion.buffer_a.glml         3881     2994   -22.9%
    reaction_diffusion.image.glml            2683     1660   -38.1%
    ripples.buffer_a.glml                    2326     1818   -21.8%
    ripples.image.glml                       3328     2086   -37.3%
    truchet.glml                             4584     3044   -33.6%
    warped_noise.glml                        6137     4647   -24.3%
    |}]
;;

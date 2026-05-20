# Overview

*Note: We assume some familiarity with GLSL and ML terminology*

A GLML program is a single pure function from a `vec2` pixel coordinate to a `vec3` color:

```glml
let main (coord : vec2) = [0.6, 0.2, 0.8]
```

The host pipes in uniforms with `#extern`, builtins are prefixed with `#`, and the rest of the language looks like a small ML, with the classic `let`, `let rec`, `match`, `fun`, records, variants, and Hindley-Milner inference. GLML supports first class functions/closures and recursion. The compiler lowers it to a fragment shader that runs anywhere WebGL2 or GLES 3.0 runs, but is primarily designed for quick Shadertoy-like development.

## Example: Mandelbrot Shader

Here is the classic Mandelbrot shader, which can be written as a terse and typesafe recursive function without any annotations:

```glml
#extern vec2 u_resolution
#extern float u_time

type option['a] = Some of 'a | None

let mandelbrot c =
  let rec mandel z i =
    if i > 150 then None
    else if #length(z) > 4 then
      let nu = #log2(#log2(#length(z))) in
      Some ((i - nu) / 150)
    else
      let zx = z.0 * z.0 - z.1 * z.1 in
      let zy = 2 * z.0 * z.1 in
      mandel ([zx, zy] + c) (i + 1)
  in
  mandel [0, 0] 0

let main (coord : vec2) =
  let uv =
    let top = 2 * coord - u_resolution in
    let bot = #min(u_resolution.0, u_resolution.1) in
    top / bot
  in
  let zoom = #exp(#sin(u_time * 0.4) * 4.5 + 3.5) in
  let seahorse_valley = [-0.7453, 0.1127] + uv / zoom in
  match mandelbrot seahorse_valley with
  | None   -> [0, 0, 0]
  | Some n -> #sin(n * [10, 20, 30] + u_time) * 0.5 + 0.5
```

## Example: GLSL vs GLML

The following is the same scene in both GLSL and GLML, where we define a SDF of a circle and rectangle.

```glsl
// kind 0 = circle (a = radius), kind 1 = rect (a, b)
struct Shape { int kind; float a; float b; };

float sdShape(Shape s, vec2 p) {
    if (s.kind == 0) return length(p) - s.a;
    if (s.kind == 1) {
        vec2 d = abs(p) - vec2(s.a, s.b);
        return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0);
    }
    return 1.0;
}

float scene(vec2 p) {
    Shape c; c.kind = 0; c.a = 0.3;
    Shape r; r.kind = 1; r.a = 0.7; r.b = 0.1;
    return min(sdShape(c, p), sdShape(r, p));
}
```
```glml
type shape = Circle of float | Rect of float * float

let sdf_shape (s : shape) = fun p ->
  match s with
  | Circle r    -> #length(p) - r
  | Rect (w, h) ->
    let d = #abs(p) - [w, h] in
    #length(#max(d, [0, 0])) + #min(#max(d.0, d.1), 0)

let union f g = fun p -> #min(f p, g p)

let scene = union (sdf_shape (Circle 0.3)) (sdf_shape (Rect (0.7, 0.1)))
```

In the GLSL version, the compiler can't check you handled every kind of `shape`. In the GLML version, `shape` is a typed variant. `match` is exhaustive, each constructor carries its own typed payload, and `union` is a function that returns a function.

Unlike most ML languages that don't provide any form of ad-hoc polymorphism, GLML provides a limited set of constraints (broadcasting over vectors and scalars, promotion of integers to floats) that enables classic HM inference without the burden of having explicit operators over various shapes.

## Example: Optimization

Each ML feature has a corresponding lowering pass that erases it before GLSL is emitted

- Variants become discriminated structs of plain scalars
- Records become bundles of locals
- Closures are defunctionalized into tagged dispatch
- Polymorphic functions are monomorphized to one specialized copy per concrete use
- Recursion is rewritten to a `while` loop

By the time the program reaches the GPU, it is straight GLSL with no allocation, no function pointers, and no runtime tag dispatch beyond what the algorithm actually needs.

Below is GLML code rendering a simple union of two SDFs, and the optimized GLSL that is generated

```glml
#extern vec2 u_resolution

type sdf = vec2 -> float

type shape =
  | Circle of float
  | Rect of float * float

let sdf_shape (s : shape) : sdf =
  fun p ->
    match s with
    | Circle r -> #length(p) - r
    | Rect (w, h) ->
      let d = #abs(p) - [w,h] in
      #length(#max(d, [0, 0])) + #min(#max(d.0, d.1), 0)
    
let union (f : sdf) (f' : sdf) : sdf =
  fun p -> #min(f p, f' p)

let scene : sdf =
  let circle = sdf_shape (Circle 0.3) in
  let rect = sdf_shape (Rect (0.7, 0.1)) in
  union circle rect

let main (coord : vec2) =
  let p = 2 * coord / u_resolution - 1 in
  let d = scene p in
  if d > 0 then [d, d, d] else [1, 1, 1] + d
```

Generated GLSL output:

```glsl
#version 300 es
precision highp float;
out vec4 fragColor;
uniform vec2 u_resolution;
vec3 main_pure(vec2 coord) {
    vec2 anf_13 = (2. * coord);
    vec2 anf_14 = (anf_13 / u_resolution);
    vec2 p_1 = (anf_14 - 1.);
    float anf_19 = length(p_1);
    float anf_11_1 = (anf_19 - 0.3);
    vec2 anf_0_1 = abs(p_1);
    vec2 anf_1_1 = vec2(0.7, 0.1);
    vec2 d_2 = (anf_0_1 - anf_1_1);
    vec2 anf_2_1 = vec2(0., 0.);
    vec2 anf_3_1 = max(d_2, anf_2_1);
    float anf_4_1 = length(anf_3_1);
    float anf_5_1 = d_2[0];
    float anf_6_1 = d_2[1];
    float anf_7_1 = max(anf_5_1, anf_6_1);
    float anf_8_1 = min(anf_7_1, 0.);
    float anf_12_1 = (anf_4_1 + anf_8_1);
    float d_0 = min(anf_11_1, anf_12_1);
    bool anf_15 = (d_0 > 0.);
    if (anf_15) {
        return vec3(d_0, d_0, d_0);
    } else {
        vec3 anf_16 = vec3(1., 1., 1.);
        return (anf_16 + d_0);
    }
}
void main() {
    vec3 color = main_pure(gl_FragCoord.xy);
    fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
}
```

Overall, we get the elegance of a functional language that is performant enough for shaders!

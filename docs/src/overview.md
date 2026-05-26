# Overview

*Note: We assume some familiarity with GLSL and ML terminology*

A GLML program is a single pure function from a `vec2` pixel coordinate to a `vec3` color:

```glml
let main (coord : vec2) = [0.6, 0.2, 0.8]
```

The host pipes in uniforms with `#extern`, builtins are prefixed with `#`, and the rest of the language looks like a small ML, with the classic `let`, `let rec`, `match`, `fun`, records, variants, and Hindley-Milner inference. GLML supports first class functions/closures and recursion. The compiler lowers it to a fragment shader that runs anywhere WebGL2 or GLES 3.0 runs, but is primarily designed for quick Shadertoy-like development.

## Example: Mandelbrot Shader

Here is the classic Mandelbrot shader, which can be written as a terse and typesafe recursive function without any annotations:

```glml,live
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

Below is GLML code rendering a simple union of two SDFs (open the optimized GLSL that is generated, the code before `vec3 base;` is all that is needed to represent the SDF!)

```glml,live
#extern vec2 u_resolution
#extern vec2 u_mouse

type sdf = vec2 -> float

type shape =
  | Circle of float
  | Rect of float * float

let sdf_shape (s : shape) : sdf =
  fun p ->
    match s with
    | Circle r -> #length(p) - r
    | Rect (w, h) ->
      let d = #abs(p) - [w, h] in
      #length(#max(d, [0, 0])) + #min(#max(d.0, d.1), 0)

let union (f : sdf) (g : sdf) : sdf = fun p -> #min(f p, g p)

let scene = union (sdf_shape (Circle 0.3)) (sdf_shape (Rect (0.7, 0.1)))

let uv coord =
  (2 * coord - u_resolution) / #min(u_resolution.0, u_resolution.1)

let main (coord : vec2) =
  let p = uv coord in
  let m = uv u_mouse in
  let d = scene p in

  let base  = if d > 0 then [0.9, 0.6, 0.3] else [0.65, 0.85, 1.0] in
  let shade = (1 - #exp(-6 * #abs(d))) * (0.8 + 0.2 * #cos(150 * d)) in
  let edge  = 1 - #smoothstep(0, 0.01, #abs(d)) in
  let dm    = #length(p - m) in
  let ds    = #abs(dm - #abs(scene m)) in
  let mouse = 1 - #smoothstep(0, 0.005, #min(ds - 0.0025, dm - 0.015)) in

  let col = base * shade in
  let col = #mix(col, [1, 1, 1], edge) in
  #mix(col, [1, 1, 0], mouse)
```


Overall, we get the elegance of a functional language that is performant enough for shaders!

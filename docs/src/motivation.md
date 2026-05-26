# Motivation

The short answer: I wanted to learn more about painting with signed distance functions, and the [yak was shaved](https://en.wiktionary.org/wiki/yak_shaving) too much. On the bright side, I got to render some nice volumetric clouds.

The long answer:

## Ad-hoc Polymorphism

The language leans in on ad-hoc polymorphism, arithmetic operators are overloaded across scalars, vectors, and matrices. Built-in math functions like `#sin`, `#min`, and `#length` accept anything that broadcasts up to a float-shaped thing. Integer literals are coerced into floats. All of these are features unavailable in most ML languages but are essential to shaders.

```glml
let get_uv coord =
  let top = 2 * coord - u_resolution in
  let bot = #min u_resolution.0 u_resolution.1 in
  top / bot
```

No annotations. `2 * coord` mixes an integer and a `vec2`, and `top / bot` divides a vector by a scalar. Each of these would need a constructor call or a different operator in OCaml. In GLML they just work because the typechecker emits a constraint (`Broadcast`, `MulBroadcast`, `Coerce`) rather than pattern-matching the operand shapes directly. The solver picks the overload and promotes the literals.

The result is that GLML feels almost untyped for the kind of code shaders are usually made of, and only asserts itself when you do something it can actually catch  (a missing `match` case, a `vec2 + vec3`, etc).

## Signed Distance Functions

An SDF is a function `vec2 -> float`, mapping a position to its distance to the object boundary defined by the SDF. In raymarchers, scenes are compositions of these, operations that act on functions. In GLML we can treat it as a type alias and a few combinators:

```glml
type sdf = vec2 -> float

let union (f : sdf) (g : sdf) : sdf = fun p -> #min (f p) (g p)
let scale s (f : sdf) : sdf         = fun p -> f (p / s)
let translate o (f : sdf) : sdf     = fun p -> f (p - o)
```

You write each combinator once and reuse it across every scene. This makes writing raymarchers elegant.

## Closeness to GLSL

GLML fundamentally needed to be a language close to GLSL, it doesn't try to be a new platform. The runtime model is unchanged, and the existing GLSL stays useful. The plan is for GLSL to be easily interop-able with GLML, dropping GLML into an existing fragment shader shouldn't be difficult.

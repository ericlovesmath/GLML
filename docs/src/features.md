# Features

This is a list of highlighted features you may not have expected to see from an ML language and/or GLSL, not an exhaustive list of features. See [Syntax](./reference/syntax.md) for proper reference.

## Bounded-Loop Recursion

`let rec` is lowered to a `while` loop with a hard 1000-iteration cap (*Note: can be disabled, but I enjoy having my GPU not lock up*).

```glml
let rec mandel z i =
  if i > 150            then None
  else if #length(z) > 4 then Some i
  else mandel (square z + c) (i + 1)
```

## Broadcasting and Overloading

Arithmetic operators are overloaded across scalars, vectors, and matrices

```glml
2 * [1.0, 2.0, 3.0]                 // [2.0, 4.0, 6.0]
rotate u_time * uv                  // mat2 * vec2 -> vec2
#sin(wave + [0, 2, 4]) * 0.3 + 0.7  // rainbow
```

## Integer Literals Promote to Float

In a context that expects a float, ints are promoted automatically (`int <: float` subtyping).

```glml
let top = 2 * coord - n       // 2 promotes, result vec(_, float)
let v   = [some_int, 2.0, 4]  // vec3 of float, not int (some_int is an int)
```

## Exhaustive Pattern Matching

`match` is exhaustive by default. Forget a case and you get a compile error pointing at the missing constructor. A catch-all `_` opts out for the cases it absorbs.

```glml
match s with
| Circle r    -> ...
| Rect (w, h) -> ...
// ERROR: missing case `Empty`
```

See [Variants and Pattern Matching](./tutorial/variants.md).

## First-Class Functions

The compiler defunctionalizes first class functions into a tag plus a capturing the minimal environment struct. The combinator pattern that makes SDF composition pleasant has zero runtime cost, since the optimizer can aggressively remove these kinds of expressions.

```glml
let union (f : sdf) (g : sdf) : sdf = fun p -> #min(f p, g p)
let scale s (f : sdf) : sdf         = fun p -> f (p / s)
```

See [First-Class Functions](./tutorial/higher-order.md).

## Type Classes and Constraints

Functions are typed by inference, but there are seven built-in classes (`GenType`, `GenIType`, `GenBType`, `MatType`, `Numeric`, `Comparable`, `Equatable`) that are not extendable. The classes describe the same types as GLSL's overload sets.

```glml
let add a b = a + b
//  ^^^ inferred: `'a -> 'b -> 'r  where ('a + 'b -> 'r)`
//      works on int+int, float+float, vec3+vec3, vec2+float, mat2+mat2

+, -       : 'a -> 'b -> 'r   where Broadcast('a, 'b, 'r)
*, /       : 'a -> 'b -> 'r   where MulBroadcast('a, 'b, 'r)
#sin(x)    : 'a -> 'b         where Broadcast('a, float, 'b), GenType('b)
```

See [Type System](./type-system.md)

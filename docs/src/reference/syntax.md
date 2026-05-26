# Syntax

```glml
//// Uniforms
// declared at the top level with `#extern`; supplied by the host
#extern vec2  u_resolution
#extern vec2  u_mouse
#extern float u_time

//// Top-level constants
let pi  = 3.14159
let sky = [0.65, 0.85, 1.0]

//// Type declarations

// records
type point     = { x: float, y: float }
type ray       = { ro: vec3, rd: vec3 }
type box['a]   = { value: 'a }

// variants
type shape =
  | Circle of float
  | Rect   of float * float
  | Empty

type option['a] = Some of 'a | None

// type aliases
type sdf   = vec2 -> float
type boxed = box[float]

//// Top-level functions
let double x = 2 * x
let double = fun x -> 2 * x    // Same as above
let add x y  = x + y

// argument and return annotations are optional
let rotate (angle : float) : mat2 =
  let s = #sin angle in
  let c = #cos angle in
  [[c, -s], [s, c]]

// recursion
let rec gcd a b =
  if b = 0 then a
  else gcd b (a - b * #floor (a / b))

//// Vector and matrix literals
let v = [1.0, 2.0, 3.0]                       // vec3
let m = [[1.0, 0.0], [0.0, 1.0]]              // mat2
let m = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]     // mat3 (ints promote)

//// Component access
let a = v.0 + v.1 + v.2
let b = { x = 1.0, y = 2.0 }.x

//// Operators
let _ = 1 + 2                 // int + int
let _ = 2 * [1.0, 2.0]        // scalar broadcasts over vector
let _ = rotate u_time * v3    // mat * vec
let _ = [1, 2] = [1, 2]       // equality (returns bool)
let _ = u_time > 0.5          // comparison
let _ = true && false         // logical

// `x |> f` is sugar for `f x`
let _ =
  sky
  |> #max 0
  |> #min [1, 1, 1]

//// Local bindings
let example coord =
  let aspect   = u_resolution.0 / u_resolution.1 in
  let uv       = coord / u_resolution in
  let [x, y]   = uv in
  let centered = uv - [0.5, 0.5] in
  [x * aspect, y, 0.0]

//// Conditionals
let band d =
  if d < 0.0      then sky
  else if d < 0.5 then [0.4, 0.3, 0.2]
  else                 [1.0, 1.0, 1.0]

//// Constructing variants and records
let s = Circle 0.3
let s = Rect (0.7, 0.1)
let s = Empty

let r : ray = { ro = [0, 0, 0], rd = [0, 0, 1] }
let b = { value = [1, 2, 3] }
let o = Some [1.0, 2.0, 3.0]
let o = None

//// Pattern matching
let area s =
  match s with
  | Circle r    -> 3.14 * r * r
  | Rect (w, h) -> w * h
  | _           -> true

//// Higher-order functions
let scale s    = fun v -> v * s
let union f g  = fun p -> #min (f p) (g p)
let twice f x  = f (f x)                  // ('a -> 'a) -> 'a -> 'a inferred

//// User-written constraints (optional, encourage to leave out to be inferred)
let add (x : 'a) (y : 'b) : 'c = x + y
  where ('a + 'b -> 'c)

// forms:
//   Num 't           -> Numeric (float, int, vec, mat)
//   'a + 'b -> 'r    -> Broadcasting (Addition)
//   'a * 'b -> 'r    -> Broadcasting (Multiplication)

//// GLSL Builtins (any `#name`)
let _ = #sin u_time
let _ = #length [3.0, 4.0]
let _ = #min 0.5 1.0
let _ = #mix [1, 0, 0] [0, 0, 1] 0.5
let _ = #cross [1, 0, 0] [0, 1, 0]
let _ = #float 2

//// Entry point must be `main : vec2 -> vec3`
let main (coord : vec2) =
  let uv = coord / u_resolution in
  let d  = #length (uv - [0.5, 0.5]) - 0.3 in
  if d < 0.0 then [1.0, 0.5, 0.2] else sky
```

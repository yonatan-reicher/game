module Maths
open Maths

// This namespace is called Maths because Math conflicts with the legacy
// FSharp.Math module.

let private Float32WithMeasure = LanguagePrimitives.Float32WithMeasure

type vec2<[<Measure>] 'a> = Vector.Vec2<'a>
type vec2 = vec2<1>
let vec2 x y: vec2<_> = { X = x; Y = y }

/// Radians
[<Measure>]
type rad

let sin (x: float32<rad>): float32 = sin (float32 x)
let cos (x: float32<rad>): float32 = cos (float32 x)
let tan (x: float32<rad>): float32 = tan (float32 x)
let asin (x: float32): float32<rad> = asin (float32 x) |> Float32WithMeasure
let acos (x: float32): float32<rad> = acos (float32 x) |> Float32WithMeasure
let atan (x: float32): float32<rad> = atan (float32 x) |> Float32WithMeasure
let atan2 (y: float32) (x: float32): float32<rad> = atan2 (float32 y) (float32 x) |> Float32WithMeasure

module Maths

// This namespace is called Maths because Math conflicts with the legacy
// FSharp.Math module.

let private Float32WithMeasure = LanguagePrimitives.Float32WithMeasure

type Vec2<[<Measure>] 'a> =
    { X: float32<'a>
      Y: float32<'a> }

    static member (+)(a: Vec2<'a>, b: Vec2<'a>) = { X = a.X + b.X; Y = a.Y + b.Y }

    static member (-)(a: Vec2<'a>, b: Vec2<'a>) = { X = a.X - b.X; Y = a.Y - b.Y }

    static member (*) (a: float32<'b>, b: Vec2<'a>) = { X = a * b.X; Y = a * b.Y }
    static member (*) (a: Vec2<'a>, b: float32<'b>) = { X = a.X * b; Y = a.Y * b }
    static member (*) (a: Vec2<'a>, b: Vec2<'b>) = { X = a.X * b.X; Y = a.Y * b.Y }


type vec2<[<Measure>] 'a> = Vec2<'a>
type vec2 = vec2<1>
let vec2 x y: vec2<_> = { X = x; Y = y }

[<Measure>]
type radian

/// Radians
[<Measure>]
type rad = radian

/// Radians
[<Measure>]
type pixel

/// Pixels
[<Measure>]
type px = pixel

// Reexport some standard units

[<Measure>]
type meter = Data.UnitSystems.SI.UnitNames.meter

[<Measure>]
type m = meter

[<Measure>]
type second = Data.UnitSystems.SI.UnitNames.second

[<Measure>]
type s = second

let sin (x: float32<rad>): float32 = sin (float32 x)
let cos (x: float32<rad>): float32 = cos (float32 x)
let tan (x: float32<rad>): float32 = tan (float32 x)
let asin (x: float32): float32<rad> = asin (float32 x) |> Float32WithMeasure
let acos (x: float32): float32<rad> = acos (float32 x) |> Float32WithMeasure
let atan (x: float32): float32<rad> = atan (float32 x) |> Float32WithMeasure
let atan2 (y: float32<'a>) (x: float32<'a>): float32<rad> = atan2 (float32 y) (float32 x) |> Float32WithMeasure

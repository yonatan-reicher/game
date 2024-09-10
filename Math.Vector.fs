module Maths.Vector

let private Float32WithMeasure = LanguagePrimitives.Float32WithMeasure

type Vec2<[<Measure>] 'a> =
    { X: float32<'a>
      Y: float32<'a> }

    static member (+)(a: Vec2<'a>, b: Vec2<'a>) = { X = a.X + b.X; Y = a.Y + b.Y }

    static member (-)(a: Vec2<'a>, b: Vec2<'a>) = { X = a.X - b.X; Y = a.Y - b.Y }

    static member (*) (a: float32<'b>, b: Vec2<'a>) = { X = a * b.X; Y = a * b.Y }
    static member (*) (a: Vec2<'a>, b: float32<'b>) = { X = a.X * b; Y = a.Y * b }


let scale (b: float32<'b>) (a: Vec2<'a>) = a * b

let right<[<Measure>] 'a> : Vec2<'a> =
    { X = Float32WithMeasure 1.0f
      Y = Float32WithMeasure 0.0f }

let up<[<Measure>] 'a> : Vec2<'a> =
    { X = Float32WithMeasure 0.0f
      Y = Float32WithMeasure 1.0f }

let left<[<Measure>] 'a> : Vec2<'a> =
    { X = Float32WithMeasure -1.0f
      Y = Float32WithMeasure 0.0f }

let down<[<Measure>] 'a> : Vec2<'a> =
    { X = Float32WithMeasure 0.0f
      Y = Float32WithMeasure -1.0f }

let zero<[<Measure>] 'a> : Vec2<'a> =
    { X = Float32WithMeasure 0.0f
      Y = Float32WithMeasure 0.0f }

let one<[<Measure>] 'a> : Vec2<'a> =
    { X = Float32WithMeasure 1.0f
      Y = Float32WithMeasure 1.0f }

let sqrLength (v: Vec2<'a>) = v.X * v.X + v.Y * v.Y

let length (v: Vec2<'a>): float32<'a> = System.MathF.Sqrt (sqrLength v |> float32) |> Float32WithMeasure

let normalize (v: Vec2<'a>): Vec2<1> =
    if sqrLength v = 0.0f<_> then zero
    else scale (1.0f / length v) v

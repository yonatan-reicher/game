module Maths.Vector

let private Float32WithMeasure = LanguagePrimitives.Float32WithMeasure


let scale (b: float32<'b>) (a: Vec2<'a>) = a * b

let dot (a: vec2<'a>) (b: vec2<'b>) = a.X * b.X + a.Y * b.Y

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

let angle (v: Vec2<'a>): float32<rad> = atan2 v.Y v.X
let fromAngle (angle: float32<rad>): Vec2<1> =
    { X = cos angle
      Y = sin angle }
      

let rotate (angle: float32<rad>) (v: Vec2<'a>): Vec2<'a> =
    let cosA = cos angle
    let sinA = sin angle
    { X = cosA * v.X - sinA * v.Y
      Y = sinA * v.X + cosA * v.Y }


let rotateAroundPoint (point: vec2<'a>) (angle: float32<rad>) (x: vec2<'a>) : vec2<'a> =
    let x' = x - point
    let rotated = rotate angle x'
    rotated + point


let moveTo (target: Vec2<'a>) (current: Vec2<'a>) (maxStep: float32<'a>): Vec2<'a> =
    let diff = target - current
    let dist = length diff
    let step = min dist maxStep
    let move = normalize diff
    current + scale step move

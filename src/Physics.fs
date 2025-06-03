module Physics


open Maths


[<Fable.Core.Mangle>]
type Position<'a> =
    abstract member Pos: vec2<m>
    abstract member WithPos: vec2<m> -> 'a

[<Fable.Core.Mangle>]
type Rotation<'a> =
    abstract member Rot: float32<rad>
    abstract member WithRot: float32<rad> -> 'a

[<Fable.Core.Mangle>]
type Velocity<'a> =
    inherit Position<'a>
    abstract member Vel: vec2<m / s>
    abstract member WithVel: vec2<m / s> -> 'a

[<Fable.Core.Mangle>]
type AngularVelocity<'a> =
    inherit Rotation<'a>
    abstract member AngVel: float32<rad / s>
    abstract member WithAngVel: float32<rad / s> -> 'a


// Compiler bug; These next two functions, when marked with `inline`, get
// inlined and the plus operator emits wrong code.

let tickVelocity (delta: float32<s>) (obj: #Velocity<'a>) : 'a = obj.WithPos(obj.Pos + delta * obj.Vel)

let tickAngularVelocity (delta: float32<s>) (obj: #AngularVelocity<'a>) : 'a =
    obj.WithRot(obj.Rot + delta * obj.AngVel)

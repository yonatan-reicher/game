module Maths.Random

open Fable.Core


[<Emit("Math.random()")>]
let float () : float = jsNative


let float32 () = float32 (float ())


let float32Between (min: float32) (max: float32) : float32 = min + (max - min) * float32 ()


let direction () =
    let angle = 2f<rad> * System.MathF.PI * float32 ()
    Vector.fromAngle angle

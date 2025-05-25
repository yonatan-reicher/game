module Maths.Random

open Fable.Core


[<Emit("Math.random()")>]
let float () : float = jsNative


let float32 () = float32 (float ())


let direction () =
    let angle = 2f<rad> * System.MathF.PI * float32 ()
    Vector.fromAngle angle

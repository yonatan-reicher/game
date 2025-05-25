module BulletTrail

open Maths
open Util

// Representation:
// A list of bullet positions revere order.
// (Reverse order so that adding is O(1)).
type Trail = Trail of vec2<meter> list


let emptyTrail: Trail = Trail []


let tick (bulletPosition: vec2<m>) : Trail -> Trail =
    fun (Trail pos) -> bulletPosition :: pos |> List.truncate 3 |> Trail


let draw (radius: float32<m>) (Trail points) (context: Rendering.CanvasContext) =
    context.fillStyle <- Fable.Core.U3.Case1 "red"
    context.strokeStyle <- Fable.Core.U3.Case1 "red"
    context.lineWidth <- float (2f * radius)

    let pairs = Seq.zip points (Seq.tryTail points |> Option.defaultValue [])

    context.beginPath ()

    for (a, b) in pairs do
        context.moveTo (float a.X, float a.Y)
        context.lineTo (float b.X, float b.Y)

    context.closePath ()
    context.stroke ()

(*
    for { X = x; Y = y } in points do
        context.ellipse (float x, float y, 5, 5, 0, 0, 2. * System.Math.PI)
        context.fill()
        *)

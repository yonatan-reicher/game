module Player

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Fable.Core

open Maths


type Player = { Position: vec2<m> }

let speed = 100.0f<m / s>
let radius = 10.0f<m>

let init () =
    (*
    let x = Rendering.getCanvas().width / 2.0
    let y = Rendering.getCanvas().height / 2.0
    *)
    let x, y = 0.0f<m>, 0.0f<m>
    { Position = vec2 (1f<m> * float32 x) (1f<m> * float32 y) }

let draw ({ Position = pos }: Player) (context: Rendering.CanvasContext) =
    let r = radius
    context.fillStyle <- U3.Case1 "black"
    context.beginPath ()
    context.ellipse (float pos.X, float pos.Y, float r, float r, 0.0, 0.0, 2.0 * System.Math.PI)
    context.fill ()

let tick ({ Position = pos } as player: Player) ({ Delta = delta }: Time.Frame) : Player =
    let anyDown list = List.exists Input.isKeyDown list

    let moveH =
        if anyDown [ Input.ArrowRight; Input.KeyD ] then
            Vector.right<m>
        else if anyDown [ Input.ArrowLeft; Input.KeyA ] then
            Vector.left<m>
        else
            Vector.zero

    let moveV =
        if anyDown [ Input.ArrowDown; Input.KeyS ] then
            Vector.down<m>
        else if anyDown [ Input.ArrowUp; Input.KeyW ] then
            Vector.up<m>
        else
            Vector.zero

    let move = Vector.normalize (moveH + moveV)
    let target = Vector.sqrLength move
    Time.timeScale.Value <- lerp Time.timeScale.Value target 0.09f
    printfn "%f" Time.timeScale.Value

    { player with
        Position = pos + (speed * delta * move) }

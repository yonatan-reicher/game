module Player

open Fable.Core

open Maths
open Game


let speed = 10.0f<m / s>
let radius = 0.6f<m>

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

let tick ({ Position = pos } as player: Player) (frame: Time.Frame) : Player =
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
    let timeMove = Vector.sqrLength move <> 0f
    // Time increases slowly and decreases quickly.
    if timeMove then
        Time.timeScale.Value <- moveTo 1f Time.timeScale.Value (frame.UnscaledDelta / 1f<s>)
    else
        Time.timeScale.Value <- moveTo 0.01f Time.timeScale.Value (frame.UnscaledDelta * 10f< / s>)

    { player with
        Position = pos + (speed * frame.Delta * move) }

module Game.Draw

open Rendering
open Maths
open Sprite
open Particle
open FSharpPlus


type private CC = CanvasContext


let private clear (context: CC) =
    context.clearRect (0, 0, context.canvas.width, context.canvas.height)


let private drawWorld (state: Level) (context: CC) =
    // Just a simple grid
    context.fillStyle <- Fable.Core.U3.Case1 "black"
    context.fillRect (-0.1, -1000, 0.2, 2000)
    context.fillRect (-1000, -0.1, 2000, 0.2)

    // Draw the state
    Player.draw state.Player context
    List.iter (fun bullet -> Bullet.draw bullet context) state.Bullets
    List.iter (fun enemy -> Enemy.draw enemy context) state.Enemies
    List.iter (fun prop -> Prop.draw prop context) state.Props
    List.iter (fun chest -> Chest.draw chest context) state.Chests
    List.iter (fun p -> Particle.draw p </Draw.action/> context) state.Particles

    // Draw the mouse for debugging.
    let x =
        Camera.screenToWorld state.Camera context (Input.getMousePosition () * 1f<_>)

    context.fillStyle <- Fable.Core.U3.Case1 "red"
    context.beginPath ()
    context.ellipse (float x.X, float x.Y, 0.5, 0.5, 0.0, 0.0, 2.0 * System.Math.PI)
    context.fill ()


let drawGameOver (context: CanvasContext) : unit =
    (Text.text "Game Over! :("
     |> Text.withSize 64
     |> Text.withColor (Fable.Core.U3.Case1 "white")
     |> Text.withAlignment Text.Align.Center
     |> Text.draw
     |> movedTo
         { X = float32 context.canvas.width * 0.5f
           Y = float32 context.canvas.height * 0.5f })
        context


let rec draw (state: State) (context: CanvasContext) =
    match state with
    | Level state ->
        clear context
        Camera.apply state.Camera context (drawWorld state)
        UI.draw state context
    | GameOver levelState ->
        draw (Level levelState) context
        context.fillStyle <- Fable.Core.U3.Case1 "rgba(0, 0, 0, 0.5)"
        context.fillRect (0.0, 0.0, context.canvas.width, context.canvas.height)
        drawGameOver context
        printfn "Done!"

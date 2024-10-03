module Game.Draw

open Rendering
open Maths
open Sprite


type private CC = CanvasContext


let private clear (context: CC) =
    context.clearRect (0, 0, context.canvas.width, context.canvas.height)


let private drawWorld (state: Level) (context: CC) =
    // Just a simple grid
    context.fillStyle <- Fable.Core.U3.Case1 "black"
    context.fillRect (-0.1, -1000, 0.2, 2000)
    context.fillRect (-1000, -0.1, 2000, 0.2)

    // Draw the state
    for prop in state.Props do
        Prop.draw prop context

    Player.draw state.Player context
    List.iter (fun bullet -> Bullet.draw bullet context) state.Bullets
    List.iter (fun enemy -> Enemy.draw enemy context) state.Enemies


    // Draw the mouse for debugging.
    let x =
        Camera.screenToWorld state.Camera context (Input.getMousePosition () * 1f<_>)

    context.fillStyle <- Fable.Core.U3.Case1 "red"
    context.beginPath ()
    context.ellipse (float x.X, float x.Y, 0.5, 0.5, 0.0, 0.0, 2.0 * System.Math.PI)
    context.fill ()


let private sprite = Sprite.fromUrl "assets/slowdown-icon.png"
let private halfSize = 25f<px>

let private drawSlowdownChip context =
    Sprite.draw
        { Position =
            { X = halfSize * 1f<m / px>
              Y = halfSize * 1f<m / px> }
          Width = 2f<m / px> * halfSize
          Rotation = 0f<rad> }
        sprite
        context

let private drawMovementSpeedChip = drawSlowdownChip
let private drawHomingBulletsChip = drawSlowdownChip


let private drawChip (chip: Chip) =
    match chip with
    | SlowdownField -> drawSlowdownChip
    | MovementSpeed -> drawMovementSpeedChip
    | HomingBullets -> drawHomingBulletsChip


let private drawUi (state: Level) (context: CanvasContext) : unit =
    // outer box: surrounding box, like borderbox
    // inner box: the box inside the outer box (padding)

    context.fillStyle <- Fable.Core.U3.Case1 "rgba(0, 0, 0, 0.2)"
    let canvasHeight = float32 context.canvas.height * 1f<px>
    let padding = 10f<px>
    let margin = 10f<px>
    let inboxHeight = 2f * halfSize
    let outboxHeight = inboxHeight + 2f * padding
    let outboxWidth = 200f<px>
    let inboxWidth = outboxWidth - 2f * padding
    context.fillRect (float margin, float (canvasHeight - margin - outboxHeight), float outboxWidth, float outboxHeight)
    context.fillStyle <- Fable.Core.U3.Case1 "black"

    List.iteri
        (fun i chip ->
            movedTo
                { X = float32 (margin + padding) + float32 i * 2f< / px> * (halfSize + padding)
                  Y = float32 (canvasHeight - margin - outboxHeight + padding) }
                (drawChip chip)
                context)
        state.Chips


let drawGameOver (context: CanvasContext) : unit =
    (text "Game Over! :("
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
        drawUi state context
    | GameOver levelState ->
        draw (Level levelState) context
        context.fillStyle <- Fable.Core.U3.Case1 "rgba(0, 0, 0, 0.5)"
        context.fillRect (0.0, 0.0, context.canvas.width, context.canvas.height)
        drawGameOver context
        printfn "Done!"

module Game.UI


open Rendering
open Maths
open Sprite


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


let draw (state: Level) (context: CanvasContext) : unit =
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

module Rendering

open Browser
open Browser.Types
open Maths

type Canvas = HTMLCanvasElement
type CanvasContext = CanvasRenderingContext2D

type RenderingOptions<'state> =
    { Draw: 'state -> CanvasContext -> unit
      State: 'state ref }

let private canvas = document.createElement ("canvas") :?> Canvas
let private context = canvas.getContext ("2d") :?> CanvasContext

let private updateSizeAndState (canvas: Canvas) =
    canvas.width <- window.innerWidth
    canvas.height <- window.innerHeight
    // Flip the canvas so Y increases upwards.
    context.scale (1.0, -1.0)
    // Offset the start of the canvas to the bottom left.
    context.translate (0.0, -float canvas.height)

do
    updateSizeAndState canvas
    canvas.style.position <- "absolute"
    canvas.style.top <- "0"
    canvas.style.left <- "0"

/// Sets up the canvas and starts a drawing loop that happens on
/// every frame.
let setup ({ Draw = draw; State = state }: RenderingOptions<'state>) =
    // Set up draw loop
    let rec drawWrapper () =
        draw state.Value context
        window.requestAnimationFrame (fun _ -> drawWrapper ()) |> ignore

    drawWrapper ()

    // Set up resize listener
    window.addEventListener ("resize", (fun _ -> updateSizeAndState canvas))

    // Append canvas to body
    document.body.appendChild (canvas) |> ignore
    document.body.style.margin <- "0"
    document.body.style.overflow <- "hidden"

let getCanvas () = canvas
let getContext () = context


let inline flippedX draw (context: CanvasContext) =
    context.scale (-1, 1)
    draw context
    context.scale (-1, 1)


let inline flippedY draw (context: CanvasContext) =
    context.scale (1, -1)
    draw context
    context.scale (1, -1)


let inline movedTo (point: vec2) draw (context: CanvasContext) =
    let x, y = float point.X, float point.Y
    context.translate (x, y)
    draw context
    context.translate (-x, -y)


let inline rotatedAround point (angle: float32<rad>) draw (context: CanvasContext) =
    let x, y = float point.X, float point.Y
    context.save()
    context.translate (x, y)
    context.rotate (float angle)
    context.translate (-x, -y)
    draw context
    context.restore()


let inline scaled (scale: float32) draw (context: CanvasContext) =
    context.save()
    context.scale (float scale, float scale)
    draw context
    context.restore()

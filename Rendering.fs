module Rendering

open Browser
open Browser.Types

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
    context.scale(1.0, -1.0)
    // Offset the start of the canvas to the center of the screen.
    context.translate (float canvas.width / 2.0, -float canvas.height / 2.0)

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

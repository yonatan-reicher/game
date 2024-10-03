module Rendering

open Browser
open Browser.Types
open Maths
open Fable.Core

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
    canvas.style.setProperty ("image-rendering", "pixelated")
    canvas.setAttribute ("imageSmoothingEnabled", "false")

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
    context.save ()
    context.translate (x, y)
    context.rotate (float angle)
    context.translate (-x, -y)
    draw context
    context.restore ()


let inline scaled (scale: float32) draw (context: CanvasContext) =
    context.save ()
    context.scale (float scale, float scale)
    draw context
    context.restore ()


module Text =

    [<RequireQualifiedAccess; Struct>]
    type Align =
        | Start
        | End
        | Left
        | Right
        | Center


    let alignToString =
        function
        | Align.Start -> "start"
        | Align.End -> "end"
        | Align.Left -> "left"
        | Align.Right -> "right"
        | Align.Center -> "center"


    type Text =
        { Text: string
          Font: string
          TextAlign: Align
          Size: int
          Color: U3<string, CanvasGradient, CanvasPattern>
          MaxWidth: float option }


    let inline text (words: string) : Text =
        { Text = words
          Font = "monospace"
          TextAlign = Align.Left
          Size = 16
          Color = U3.Case1 "black"
          MaxWidth = None }


    let inline withFont font (text: Text) = { text with Font = font }


    let withAlignment alignment (text: Text) = { text with TextAlign = alignment }


    let inline withSize size (text: Text) = { text with Size = size }


    let inline withColor color (text: Text) = { text with Color = color }


    let inline withMaxWidth maxWidth (text: Text) = { text with MaxWidth = Some maxWidth }


    let inline draw (text: Text) (context: CanvasContext) =
        context.font <- sprintf "%dpx %s" text.Size text.Font
        context.textAlign <- alignToString text.TextAlign
        context.fillStyle <- text.Color

        flippedY
            (fun context ->
                match text.MaxWidth with
                | None -> context.fillText (text.Text, 0.0, 0.0)
                | Some maxWidth -> context.fillText (text.Text, 0.0, 0.0, maxWidth))
            context


type Text = Text.Text

let text = Text.text

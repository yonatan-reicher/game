open Browser
open Browser.Types
open FSharp.Data.UnitSystems.SI.UnitSymbols

type Canvas = HTMLCanvasElement
type CanvasContext = CanvasRenderingContext2D

let FloatWithMeasure = LanguagePrimitives.FloatWithMeasure

[<Measure>]
type frame

[<Measure>]
type ms

let msInS: int<ms/s> = 1000<ms/s>
let sInMs: float<s/ms> = 1. / FloatWithMeasure (float msInS)

module Canvas = 
    let canvas = document.createElement("canvas") :?> Canvas
    let context = canvas.getContext("2d") :?> CanvasContext
    let updateSize (canvas: Canvas) =
        canvas.width <- window.innerWidth
        canvas.height <- window.innerHeight

    do
        updateSize canvas

    let initialize init tick draw =
        let state = ref init

        // Set up draw loop
        let rec drawWrapper () =
            draw state.Value context
            window.requestAnimationFrame (fun _ -> drawWrapper ())
            |> ignore
        drawWrapper ()

        // Set up tick loop
        let frameRate = 60<frame/s>
        window.setInterval((fun _ -> state.Value <- tick state.Value),
                           int(msInS / frameRate)) |> ignore

        // Set up resize listener
        window.addEventListener("resize", fun _ -> updateSize canvas)

        // Append canvas to body
        document.body.appendChild(canvas) |> ignore


type State = State of string

let init = State "a"

let tick = function
    | State s -> State (s + "a")

let draw (state: State) (context: CanvasContext) =
    match state with
    | State s -> context.fillText(s, 10., 10.)

Canvas.initialize init tick draw


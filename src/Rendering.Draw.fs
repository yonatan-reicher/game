namespace Rendering


open Fable.Core


type 'a Draw =
    private
    | Draw of (CanvasContext -> 'a) // This does a side effect

    static member inline Return x = Draw(fun _ -> x)

    static member inline (>>=)(x, f: 'a -> 'b Draw) =
        let (Draw x) = x

        Draw(fun context ->
            let result = x context
            let (Draw f) = f result
            f context)

    member this.Action =
        let (Draw action) = this
        action

    static member FromAction(action: CanvasContext -> 'a) = Draw action


[<RequireQualifiedAccess>]
module Draw =
    let inline action (x: 'a Draw) = x.Action

    let inline fromAction action : 'a Draw = Draw.FromAction action

    let width: float32 Draw = fromAction (fun context -> float32 context.canvas.width)

    let height: float32 Draw = fromAction (fun context -> float32 context.canvas.height)

    /// Clears the entire screen.
    let clear: unit Draw =
        fromAction (fun context ->
            let w = context.canvas.width
            let h = context.canvas.height
            context.clearRect (0.0, 0.0, w, h))

    /// Draws a rectangle with the given parameters.
    let rect (x: float32, y: float32, width: float32, height: float32, fillStyle: string) : unit Draw =
        fromAction (fun context ->
            context.fillStyle <- U3.Case1 fillStyle
            context.rect (float x, float y, float width, float height))

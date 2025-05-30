namespace Rendering


open Maths
open Fable.Core


type Draw<[<Measure>] 'u, 'a> =
    private
    | Draw of (CanvasContext -> 'a) // This does a side effect

    static member inline Return x : Draw<'u, 'a> = Draw(fun _ -> x)

    static member inline (>>=)(x, f: 'a -> Draw<'u, 'b>) =
        let (Draw x) = x

        Draw(fun context ->
            let result = x context
            let (Draw f) = f result
            f context)

    member this.Action =
        let (Draw action) = this
        action

    static member FromAction(action: CanvasContext -> 'a) : Draw<'u, 'a> = Draw action


[<RequireQualifiedAccess>]
module Draw =
    let inline action (x: Draw<_, _>) = x.Action

    let inline fromAction action = Draw.FromAction action

    let size: Draw<px, Vec2<px>> =
        fromAction (fun context ->
            { X = float32 context.canvas.width * 1f<px>
              Y = float32 context.canvas.height * 1f<px> })

    let width: Draw<px, float32> =
        fromAction (fun context -> float32 context.canvas.width)

    let height: Draw<px, float32> =
        fromAction (fun context -> float32 context.canvas.height)

    /// Clears the entire screen.
    let clear: Draw<px, unit> =
        fromAction (fun context ->
            let w = context.canvas.width
            let h = context.canvas.height
            context.clearRect (0.0, 0.0, w, h))

    /// Draws a rectangle with the given parameters.
    let rect
        (x: float32<'m>, y: float32<'m>, width: float32<'m>, height: float32<'m>, fillStyle: string)
        : Draw<'m, unit> =
        fromAction (fun context ->
            context.fillStyle <- U3.Case1 fillStyle
            context.fillRect (float x, float y, float width, float height))

    let moveBy (pos: vec2<'m>) (draw: Draw<'m, 'a>) : Draw<'m, 'a> =
        fromAction (fun context ->
            let (Draw action) = draw
            context.save()
            context.translate(float pos.X, float pos.Y)
            let result = action context
            context.restore()
            result)

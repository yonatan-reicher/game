module Camera

open Maths

type Camera =
    { Position: vec2<meter>
      Rotation: float32<rad>
      Width: float32<meter> }


let init =
    { Position = Vector.zero
      Rotation = 0.0f<rad>
      Width = 500.0f<m> }


let height { Width = w } (context: Rendering.CanvasContext) : float32<meter> =
    let scale = w / float32 context.canvas.width
    scale * float32 context.canvas.height


let size (camera: Camera) (context: Rendering.CanvasContext) : vec2<meter> =
    vec2 camera.Width (height camera context)


let metersPerPixel ({ Width = width }: Camera) (context: Rendering.CanvasContext) : float32<meter / px> =
    let pixels = float32 context.canvas.width * 1f<px>
    // For example, in a 500m wide camera, 1000 pixel screen, you have 0.5m per pixel.
    width / pixels


let apply (camera: Camera) (context: Rendering.CanvasContext) draw: unit =
    // Scale to the camera width.
    let scale = float32 context.canvas.width * 1f<m> / camera.Width
    let moveTo = -camera.Position * 1f< / m> + 0.5f * size camera context * 1f< / m>

    Rendering.scaled scale (Rendering.movedTo moveTo (Rendering.rotatedAround Vector.zero -camera.Rotation draw)) context


let screenToWorld (camera: Camera) (context: Rendering.CanvasContext) : vec2<px> -> vec2<m> =
    let w = 1f<px> * float32 context.canvas.width
    let h = 1f<px> * float32 context.canvas.height

    fun (screen: vec2<px>) ->
        { X = screen.X - 0.5f * w
          Y = 0.5f * h - screen.Y }
        * metersPerPixel camera context
        + camera.Position
        |> Vector.rotateAroundPoint camera.Position camera.Rotation


let worldToScreen (camera: Camera) (context: Rendering.CanvasContext) : vec2<m> -> vec2<px> =
    let w = 1f<px> * float32 context.canvas.width
    let h = 1f<px> * float32 context.canvas.height

    fun (world: vec2<m>) ->
        world
        |> Vector.rotateAroundPoint camera.Position (-camera.Rotation)
        |> fun p -> p - camera.Position
        |> fun p -> p / metersPerPixel camera context
        |> fun p ->
            { X = p.X + 0.5f * w
              Y = 0.5f * h - p.Y }

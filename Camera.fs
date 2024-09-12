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


let height { Width = w } (context: Rendering.CanvasContext): float32<meter> =
    let scale = w / float32 context.canvas.width
    scale * float32 context.canvas.height


let metersPerPixel ({Width = width}: Camera) (context: Rendering.CanvasContext): float32<meter / px> =
    let pixels = float32 context.canvas.width * 1f<px>
    // For example, in a 500m wide camera, 1000 pixel screen, you have 0.5m per pixel.
    width / pixels


let private scaleAtCenter (camera: Camera) scale (context: Rendering.CanvasContext) =
    let w = float context.canvas.width
    let h = float context.canvas.height
    context.translate (-0.5 * w, -0.5 * h)
    context.scale (scale, scale)
    context.translate (0.5 * float camera.Width, 0.5 * float (height camera context))


let apply (camera: Camera) (context: Rendering.CanvasContext) cont =
    context.save ()
    // Scale to the camera width.
    let scale = context.canvas.width / float camera.Width
    scaleAtCenter camera scale context
    // Go to camera rotation.
    context.rotate (-float camera.Rotation)
    // Go to the position.
    context.translate (-float camera.Position.X, -float camera.Position.Y)
    // Run
    cont context
    // Reset the canvas.
    context.restore ()


let private rotateAroundPoint (point: vec2<meter>) (angle: float32<rad>) (x: vec2<meter>) : vec2<meter> =
    let x' = x - point
    let rotated = Vector.rotate angle x'
    rotated + point


let screenToWorld (camera: Camera) (context: Rendering.CanvasContext) : vec2<px> -> vec2<m> =
    let w = 1f<px> * float32 context.canvas.width
    let h = 1f<px> * float32 context.canvas.height

    fun (screen: vec2<px>) ->
        { X = screen.X - 0.5f * w
          Y = 0.5f * h - screen.Y }
        * metersPerPixel camera context
        + camera.Position
        |> rotateAroundPoint camera.Position camera.Rotation


let worldToScreen (camera: Camera) (context: Rendering.CanvasContext) : vec2<m> -> vec2<px> =
    let w = 1f<px> * float32 context.canvas.width
    let h = 1f<px> * float32 context.canvas.height

    fun (world: vec2<m>) ->
        world
        |> rotateAroundPoint camera.Position (-camera.Rotation)
        |> fun p -> p - camera.Position
        |> fun p -> p / metersPerPixel camera context
        |> fun p ->
            { X = p.X + 0.5f * w
              Y = 0.5f * h - p.Y }

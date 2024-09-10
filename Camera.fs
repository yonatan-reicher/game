module Camera

open Maths

type Camera =
    { Position: vec2<meter>
      Rotation: float32<rad> }


let init =
    { Position = Vector.zero
      Rotation = 0.0f<rad> }


let apply (camera: Camera) (context: Rendering.CanvasContext) cont =
    context.save ()
    // Go to the position.
    context.translate (-float camera.Position.X, -float camera.Position.Y)
    // Go to camera rotation.
    context.rotate (-float camera.Rotation)
    // Offset the center of the canvas to the center of the camera.
    // context.translate (float context.canvas.width / 2.0, float context.canvas.height / 2.0)
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
        // TODO: Change if we ever add zoom.
        let zoom = 1.0f<m / px>

        { X = screen.X - 0.5f * w
          Y = 0.5f * h - screen.Y }
        * zoom
        + camera.Position
        |> rotateAroundPoint camera.Position camera.Rotation

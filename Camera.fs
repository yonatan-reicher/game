module Camera

open Maths
open FSharp.Data.UnitSystems.SI.UnitNames

type Camera = 
    { Position: vec2<meter>
      Rotation: float32<rad> }

let init = { Position = Vector.zero; Rotation = 0.0f<rad> }

let apply (camera: Camera) (context: Rendering.CanvasContext) cont =
    context.save()
    // Go to the position.
    context.translate (-float camera.Position.X, -float camera.Position.Y)
    // Go to camera rotation.
    context.rotate (-float camera.Rotation)
    // Offset the center of the canvas to the center of the camera.
    // context.translate (float context.canvas.width / 2.0, float context.canvas.height / 2.0)
    // Run
    cont context
    // Reset the canvas.
    context.restore()

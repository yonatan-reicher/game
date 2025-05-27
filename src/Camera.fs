module Camera

open Maths

type Camera =
    { TargetPosition: vec2<meter>
      ActualPosition: vec2<meter>
      TargetRotation: float32<rad>
      ActualRotation: float32<rad>
      Width: float32<meter>
      Shake: float32 }


let init =
    { TargetPosition = Vector.zero
      ActualPosition = Vector.zero
      TargetRotation = 0.0f<rad>
      ActualRotation = 0.0f<rad>
      Width = 30.0f<m>
      Shake = 0.0f }


let height { Width = w } (context: Rendering.CanvasContext) : float32<meter> =
    let scale = w / float32 context.canvas.width
    scale * float32 context.canvas.height


let size (camera: Camera) (context: Rendering.CanvasContext) : vec2<meter> =
    vec2 camera.Width (height camera context)


let metersPerPixel ({ Width = width }: Camera) (context: Rendering.CanvasContext) : float32<meter / px> =
    let pixels = float32 context.canvas.width * 1f<px>
    // For example, in a 500m wide camera, 1000 pixel screen, you have 0.5m per pixel.
    width / pixels


let apply (camera: Camera) (context: Rendering.CanvasContext) draw : unit =
    // Scale to the camera width.
    let scale = float32 context.canvas.width * 1f<m> / camera.Width

    let moveTo =
        -camera.ActualPosition * 1f< / m> + 0.5f * size camera context * 1f< / m>

    (draw
     |> Rendering.rotatedAround Vector.zero -camera.ActualRotation
     |> Rendering.movedTo moveTo
     |> Rendering.scaled scale)
        context


let screenToWorld (camera: Camera) (context: Rendering.CanvasContext) : vec2<px> -> vec2<m> =
    let w = 1f<px> * float32 context.canvas.width
    let h = 1f<px> * float32 context.canvas.height

    fun (screen: vec2<px>) ->
        { X = screen.X - 0.5f * w
          Y = 0.5f * h - screen.Y }
        * metersPerPixel camera context
        + camera.ActualPosition
        |> Vector.rotateAroundPoint camera.ActualPosition camera.ActualRotation


let worldToScreen (camera: Camera) (context: Rendering.CanvasContext) : vec2<m> -> vec2<px> =
    let w = 1f<px> * float32 context.canvas.width
    let h = 1f<px> * float32 context.canvas.height

    fun (world: vec2<m>) ->
        world
        |> Vector.rotateAroundPoint camera.ActualPosition (-camera.ActualRotation)
        |> fun p -> p - camera.ActualPosition
        |> fun p -> p / metersPerPixel camera context
        |> fun p ->
            { X = p.X + 0.5f * w
              Y = 0.5f * h - p.Y }


let private lerpToTarget dt (camera: Camera) : Camera =
    { camera with
        ActualPosition = Vector.lerp camera.ActualPosition camera.TargetPosition (dt * 5f< / s>)
        ActualRotation = Maths.lerp camera.ActualRotation camera.TargetRotation (dt * 1f< / s>) }


let private doShake dt (camera: Camera) : Camera =
    { camera with
        ActualPosition = camera.ActualPosition + Random.direction () * camera.Shake * 5f<m / s> * dt
        ActualRotation = camera.ActualRotation + Random.float32Between -1f 1f * camera.Shake * 0.1f<rad / s> * dt }


let private decayShake dt (camera: Camera) : Camera =
    { camera with
        Shake = Maths.lerp camera.Shake 0.0f (dt * 5f< / s>) }


let tick (dt: float32<s>) (camera: Camera) : Camera =
    camera |> lerpToTarget dt |> doShake dt |> decayShake dt


let shake (amount: float32) (camera: Camera) : Camera =
    { camera with Shake = camera.Shake + amount }

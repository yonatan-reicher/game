open FSharp.Data.UnitSystems.SI.UnitSymbols

open Rendering
open Time
open Maths

type State =
    { Player: Player.Player
      Bullets: Bullet.Bullet list
      Camera: Camera.Camera }


let init =
    { Player = Player.init ()
      Bullets = []
      Camera = Camera.init }


let constrainCameraToPlayer (state: State) =
    { state with
        Camera =
            { state.Camera with
                Position = state.Player.Position } }


let tick (state: State) ({ Delta = delta } as ft: Frame) =
    { Player = Player.tick state.Player ft
      Bullets = List.map (Bullet.tick ft) state.Bullets
      Camera = state.Camera }
    |> constrainCameraToPlayer
//  |> fun s -> { s with Camera = { s.Camera with Width = Time.getElapsed() * 300f<m/s> + float32 (getCanvas ()).width * 1f<m> } }
// |> fun s -> { s with Camera = { s.Camera with Rotation = s.Camera.Rotation + 0.001f<rad> } }


let mouseDown (pos: vec2<px>) (state: State) =
    let worldPos = Camera.screenToWorld state.Camera (getContext ()) pos

    let bullet: Bullet.Bullet =
        { Position = state.Player.Position
          Angle = Vector.angle (worldPos - state.Player.Position)
          Speed = 100.0f<m / s> }

    { state with
        Bullets = bullet :: state.Bullets }


let draw (state: State) (context: CanvasContext) =
    let w = float context.canvas.width
    let h = float context.canvas.height
    context.clearRect (-0.5 * w, -0.5 * h, w, h)

    Camera.apply state.Camera context
    <| fun context ->
        (
         // Just a simple grid
         context.fillRect (-1, -1000, 2, 2000)
         context.fillRect (-1000, -1, 2000, 2)
         Player.draw state.Player context
         List.iter (fun bullet -> Bullet.draw bullet context) state.Bullets


         // Draw the mouse for debugging.
         let x =
             Camera.screenToWorld state.Camera context (Input.getMousePosition () * 1f<_>)

         context.fillStyle <- Fable.Core.U3.Case1 "red"
         context.beginPath ()
         context.ellipse (float x.X, float x.Y, 5.0, 5.0, 0.0, 0.0, 2.0 * System.Math.PI)
         context.fill ())


let state = ref init
Rendering.setup { Draw = draw; State = state }


Time.setup
    { FrameRate = 60.0f<frame / s>
      Tick = tick
      State = state }


Input.setup
    { State = state
      OnMouseDown = mouseDown
      OnMouseUp = (fun _ -> id) }

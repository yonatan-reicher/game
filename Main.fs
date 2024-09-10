open FSharp.Data.UnitSystems.SI.UnitSymbols

open Rendering
open Time

type State =
    { Player: Player.Player 
      Camera: Camera.Camera }


let init = { Player = Player.init ()
             Camera = Camera.init }


let constrainCameraToPlayer (state: State) =
    { state with Camera = { state.Camera with Position = state.Player.Position } }


let tick (state: State) ({ Delta = delta } as ft: FrameTime) =
    { Player = Player.tick state.Player ft 
      Camera = state.Camera }
    |> constrainCameraToPlayer


let draw (state: State) (context: CanvasContext) =
    let w = float context.canvas.width
    let h = float context.canvas.height
    context.clearRect (-0.5 * w , -0.5 * h, w, h)
    Camera.apply state.Camera context <| fun context -> (
        // Just a simple grid
        context.fillRect(-1, -1000, 2, 2000)
        context.fillRect(-1000, -1, 2000, 2)
        Player.draw state.Player context
    )


let state = ref init
Rendering.setup { Draw = draw; State = state }


Time.setup
    { FrameRate = 60.0f<frame / s>
      Tick = tick
      State = state }

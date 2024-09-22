open Rendering
open Time
open Game
open Maths

let init =
    { Player = Player.init ()
      Props = [
          { Position = 1f<m> * vec2 10f 15f
            Rotation = 1f<rad>
            Width = 1f<m>
            Sprite = Sprite.Sprite.fromUrl "build/images/chair.png"
          }
      ]
      Bullets = []
      BulletTrail = BulletTrail.empty
      Camera = Camera.init 
      Enemies = [
          Enemy.initAt (vec2 2f<m> 3f<m>)
        ] }


let constrainCameraToPlayer (state: State) =
    { state with
        Camera =
            { state.Camera with
                Position = state.Player.Position } }


let doTrails (state: State) : State =
    { state with
        BulletTrail =
            BulletTrail.tick
                { PlayerPosition = state.Player.Position
                  State = state.BulletTrail
                  BulletPositions = seq { for b in state.Bullets -> (b.TrailId, Bullet.backPosition b) } } }


let tick (state: State) ({ Delta = delta } as ft: Frame) =
    { state with
        Player = Player.tick state.Player ft
        Bullets = List.map (Bullet.tick ft) state.Bullets
        Enemies = List.map (Enemy.tick ft state.Player.Position) state.Enemies }
    |> doTrails
    |> constrainCameraToPlayer
//  |> fun s -> { s with Camera = { s.Camera with Width = Time.getElapsed() * 300f<m/s> + float32 (getCanvas ()).width * 1f<m> } }
// |> fun s -> { s with Camera = { s.Camera with Rotation = s.Camera.Rotation + 0.001f<rad> } }


let mouseDown (pos: vec2<px>) (state: State) =
    let worldPos = Camera.screenToWorld state.Camera (getContext ()) pos

    let bullet: Bullet =
        { Position = state.Player.Position
          Angle = Vector.angle (worldPos - state.Player.Position)
          Speed = 60.0f<m / s>
          TrailId = BulletTrail.newTrail () }

    { state with
        Bullets = bullet :: state.Bullets }


let draw (state: State) (context: CanvasContext) =
    let size = Camera.size state.Camera context
    let w = float size.X
    let h = float size.Y

    Camera.apply state.Camera context
    <| fun context ->
        (Rendering.movedTo // Clearing the screen
            (state.Camera.Position * 1f< / m>)
            (fun context -> context.clearRect (-0.5 * w, -0.5 * h, w, h))
            context
         // Just a simple grid
         context.fillRect (-0.1, -1000, 0.2, 2000)
         context.fillRect (-1000, -0.1, 2000, 0.2)

         // Draw the state
         for prop in state.Props do
             Prop.draw prop context
         Player.draw state.Player context
         List.iter (fun bullet -> Bullet.draw bullet context) state.Bullets
         BulletTrail.draw Bullet.radius state.BulletTrail context
         List.iter (fun enemy -> Enemy.draw enemy context) state.Enemies


         // Draw the mouse for debugging.
         let x =
             Camera.screenToWorld state.Camera context (Input.getMousePosition () * 1f<_>)

         context.fillStyle <- Fable.Core.U3.Case1 "red"
         context.beginPath ()
         context.ellipse (float x.X, float x.Y, 0.5, 0.5, 0.0, 0.0, 2.0 * System.Math.PI)
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

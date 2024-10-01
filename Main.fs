open Rendering
open Time
open Game
open Maths
open Util

let init =
    { Player = Player.init ()
      Props =
        [ { Position = 1f<m> * vec2 10f 15f
            Rotation = 1f<rad>
            Width = 1f<m>
            Sprite = Sprite.Sprite.fromUrl "build/images/chair.png" } ]
      Bullets = []
      Camera = Camera.init
      Enemies =
        [ Enemy.initAt (vec2 2f<m> 3f<m>)
          Enemy.initAt (vec2 5f<m> -1f<m>)
          Enemy.initAt (vec2 -2f<m> -2f<m>)

          Enemy.initAt (vec2 4f<m> 9f<m>)
          Enemy.initAt (vec2 2f<m> -7f<m>) ]
      Chips = [] }


let mouseDown (pos: vec2<px>) (state: State) =
    let worldPos = Camera.screenToWorld state.Camera (getContext ()) pos

    let bullet: Bullet =
        { Position = state.Player.Position
          Angle = Vector.angle (worldPos - state.Player.Position)
          Speed = 60.0f<m / s>
          State = Moving
          Trail = BulletTrail.emptyTrail }

    Time.timeScale.Value <- 1f

    let dir =
        if (worldPos - state.Player.Position).X > 0f<_> then
            Right
        else
            Left

    { state with
        Bullets = bullet :: state.Bullets
        Player = { state.Player with Direction = dir } }


let private toggleChip (chip: Chip) (state: State) = 
    { state with
        Chips =
            match
                List.tryFindAndRest ((=) chip) state.Chips
            with
            | Some(_, rest) -> rest
            | None -> chip :: state.Chips }


let keyUp (key: Input.Key) (state: State) =
    match key with
    | Input.Digit1 -> toggleChip SlowdownField state
    | Input.Digit2 -> toggleChip MovementSpeed state
    | _ -> state


let draw (state: State) (context: CanvasContext) = Draw.draw state context


let state = ref init
Rendering.setup { Draw = draw; State = state }


Time.setup
    { FrameRate = 60.0f<frame / s>
      Tick = Tick.tick
      State = state }


Input.setup
    { State = state
      OnMouseDown = mouseDown
      OnMouseUp = (fun _ -> id)
      OnKeyUp = keyUp
      OnKeyDown = (fun _key -> id) }

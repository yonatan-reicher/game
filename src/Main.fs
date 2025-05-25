open Rendering
open Time
open Game
open Maths
open Util

let init =
    Level
        { Player = Player.init ()
          Props =
            [ { Position = 1f<m> * vec2 10f 15f
                Rotation = 1f<rad>
                Width = 1f<m>
                Sprite = Sprite.Sprite.fromUrl "assets/chair.png" } ]
          Chests =
            [ { Position = vec2 2f<m> 2f<m>
                Opened = false
                Chip = SlowdownField } ]
          Bullets = []
          Camera = Camera.init
          Enemies =
            [ Enemy.initAt (vec2 2f<m> 3f<m>)
              Enemy.initAt (vec2 5f<m> -1f<m>)
              Enemy.initAt (vec2 -2f<m> -2f<m>)

              Enemy.initAt (vec2 4f<m> 9f<m>)
              Enemy.initAt (vec2 2f<m> -7f<m>) ]
          Chips = [] }


let private mouseDownLevel (pos: vec2<px>) (level: Level) =
    let worldPos = Camera.screenToWorld level.Camera (getContext ()) pos

    let bullet: Bullet =
        { Position = level.Player.Position
          Angle = Vector.angle (worldPos - level.Player.Position)
          Speed = 60.0f<m / s>
          State = Moving
          Trail = BulletTrail.emptyTrail }

    Time.timeScale.Value <- 1f

    let dir =
        if (worldPos - level.Player.Position).X > 0f<_> then
            Right
        else
            Left

    { level with
        Bullets = bullet :: level.Bullets
        Player = { level.Player with Direction = dir } }


let private mouseDown pos state =
    match state with
    | GameOver _ -> state
    | Level l -> Level(mouseDownLevel pos l)


let private toggleChip (chip: Chip) (state: Level) =
    { state with
        Chips =
            match List.tryFindAndRest ((=) chip) state.Chips with
            | Some(_, rest) -> rest
            | None -> chip :: state.Chips }


let keyUp (key: Input.Key) (state: State) =
    match state with
    | Level level ->
        Level(
            match key with
            | Input.Digit1 -> toggleChip SlowdownField level
            | Input.Digit2 -> toggleChip MovementSpeed level
            | _ -> level
        )
    | GameOver _ -> state


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

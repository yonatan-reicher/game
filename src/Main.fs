(*
This file is the entry point for the game. It first creates an initial game
state. Then it connects some input and output handlers which "do" the game.
*)


open Rendering
open Time
open Game
open Maths
open Util


let init =
    Level
        { Player = Player.init ()
          ShootingState = ShootingState.Idle
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

    let (bullets, shootingState) =
        ShootingState.shoot level.Player.Position worldPos level.ShootingState
        |> function
            | Some(bullet, state) -> (bullet :: level.Bullets, state)
            | None -> (level.Bullets, level.ShootingState)

    Time.timeScale.Value <- 1f

    let dir =
        if (worldPos - level.Player.Position).X > 0f<_> then
            Right
        else
            Left

    { level with
        Bullets = bullets
        ShootingState = shootingState
        Player = { level.Player with Direction = dir }
        Camera = Camera.shake 0.5f level.Camera }


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


let private setupRendering state =
    Rendering.setup { State = state; Draw = Draw.draw }
let private setupTime state =
    Time.setup
        { FrameRate = 60.0f<frame / s>
          Tick = Tick.tick
          State = state }
let private setupInput state =
    Input.setup
        { State = state
          OnMouseDown = mouseDown
          OnMouseUp = (fun _ -> id)
          OnKeyUp = keyUp
          OnKeyDown = (fun _key -> id) }


/// This variable holds the current state of the game in any given moment.
let state = ref init
setupRendering state
setupTime state
setupInput state

module Player

open Fable.Core
open Maths
open Game
open Util


let speed = 10.0f<m / s>
let radius = 0.6f<m>

let init () =
    (*
    let x = Rendering.getCanvas().width / 2.0
    let y = Rendering.getCanvas().height / 2.0
    *)
    let x, y = 0.0f<m>, 0.0f<m>

    { Position = vec2 (1f<m> * float32 x) (1f<m> * float32 y)
      Direction = Left
      State = PlayerState.Idle }

open Sprite

module Sprites =
    let idle = Sprite.fromUrl "assets/player.png"

    let walk =
        [| Sprite.fromUrl "assets/player-walk-1.png"
           Sprite.fromUrl "assets/player-walk-2.png" |]


let private stateSprite =
    function
    | PlayerState.Walk(frame = frame) -> Sprites.walk.[frame]
    | PlayerState.Idle -> Sprites.idle


let draw
    ({ Position = pos
       Direction = dir
       State = state }: Player)
    (context: Rendering.CanvasContext)
    =
    let r = radius
    context.fillStyle <- U3.Case1 "black"
    let sprite = stateSprite state |> if Direction.isLeft dir then Sprite.flipX else id

    Sprite.draw
        { Position = pos
          Width = 2f * r
          Rotation = 0f<_> }
        sprite
        context
(*
    context.beginPath ()
    context.ellipse (float pos.X, float pos.Y, float r, float r, 0.0, 0.0, 2.0 * System.Math.PI)
    context.fill ()
    *)


let advanceFrame frame array = (frame + 1) % Array.length array


let private advanceState (tryingToWalk: bool) (ft: Time.Frame) : PlayerState -> PlayerState =
    let initialFrameDelay = 0.07f<s>

    let idle = PlayerState.Idle

    let walk frame =
        PlayerState.Walk(initialFrameDelay, frame)

    function
    | PlayerState.Idle -> if tryingToWalk then walk 0 else idle
    | PlayerState.Walk(frameDuration = frameDuration; frame = frame) ->
        match frameDuration - ft.Delta with
        | Positive as duration' -> PlayerState.Walk(duration', frame)
        | _ ->
            if tryingToWalk then
                walk (advanceFrame frame Sprites.walk)
            else
                idle


let tick chips ({ Position = pos } as player: Player) (ft: Time.Frame) : Player =
    let anyDown list = List.exists Input.isKeyDown list

    let moveH =
        if anyDown [ Input.ArrowRight; Input.KeyD ] then
            Vector.right<m>
        else if anyDown [ Input.ArrowLeft; Input.KeyA ] then
            Vector.left<m>
        else
            Vector.zero

    let moveV =
        if anyDown [ Input.ArrowDown; Input.KeyS ] then
            Vector.down<m>
        else if anyDown [ Input.ArrowUp; Input.KeyW ] then
            Vector.up<m>
        else
            Vector.zero

    let move = Vector.normalize (moveH + moveV)
    let isMoving = Vector.sqrLength move <> 0f
    // Time increases slowly and decreases quickly.
    if isMoving then
        Time.timeScale.Value <- moveTo 1f Time.timeScale.Value (ft.UnscaledDelta / 1f<s>)
    else
        Time.timeScale.Value <- moveTo 0.007f Time.timeScale.Value (ft.UnscaledDelta * 10f< / s>)

    // Speed is modified by MovementSpeed chip
    let speed' =
        if
            List.exists
                (function
                | MovementSpeed -> true
                | _ -> false)
                chips
        then

            1.8f * speed
        else
            1f * speed

    { player with
        Position = pos + (speed' * ft.Delta * move)
        Direction = Direction.fromSign move.X |> Option.defaultValue player.Direction
        State = advanceState isMoving ft player.State }


let circle (player: Player): Circle<m> =
    { Center = player.Position
      Radius = radius }

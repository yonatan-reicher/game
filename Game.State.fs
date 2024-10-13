namespace Game

open Maths
open Sprite


[<Struct>]
type Direction =
    | Left
    | Right


// TODO: Move this away
module Direction =
    let inline fromSign x =
        if sign x > 0 then Some Right
        elif sign x < 0 then Some Left
        else None

    let isLeft =
        function
        | Left -> true
        | Right -> false

    let isRight =
        function
        | Left -> false
        | Right -> true


type Chip =
    | SlowdownField
    | HomingBullets
    | MovementSpeed


// TODO: Move this away
module Chip =
    module SlowdownField =
        let radius = 3f<m>
        let radiusSqr = radius * radius
        let slowdown = 0.5f


[<RequireQualifiedAccess>]
type PlayerState =
    | Walk of frameDuration: float32<s> * frame: int
    | Idle


type Player =
    { Position: vec2<m>
      Direction: Direction
      State: PlayerState }


type BulletState =
    | ExitingCollision
    | Moving


type Bullet =
    { Position: vec2<m>
      Angle: float32<rad>
      Speed: float32<m / s>
      State: BulletState
      Trail: BulletTrail.Trail }


type Enemy =
    { Position: vec2<m>
      ShootDelay: float32<s> }


type Prop =
    { Position: vec2<meter>
      Width: float32<meter>
      Rotation: float32<rad>
      Sprite: Sprite }


type Chest =
    { Position: vec2<meter>
      Opened: bool
      Chip: Chip }


type Level =
    { Player: Player
      Bullets: Bullet list
      Camera: Camera.Camera
      Props: Prop list
      Chests: Chest list
      // The equiped chips. Not inside the player structure because we could
      // for example have the game inside a shop menu or something like that.
      Chips: Chip list
      Enemies: Enemy list }


type State = 
    | Level of Level
    | GameOver of Level

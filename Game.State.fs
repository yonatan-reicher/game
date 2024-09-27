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


type State =
    { Player: Player
      Bullets: Bullet list
      Camera: Camera.Camera
      Props: Prop list
      Enemies: Enemy list }

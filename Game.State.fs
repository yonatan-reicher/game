namespace Game

open Maths
open Sprite


type Player = { Position: vec2<m> }


type BulletState =
    | ExitingShooter
    | Moving


type Bullet =
    { Position: vec2<m>
      Angle: float32<rad>
      Speed: float32<m/s>
      State: BulletState
      TrailId: BulletTrail.TrailId }


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
      BulletTrail: BulletTrail.State
      Camera: Camera.Camera
      Props: Prop list
      Enemies: Enemy list }

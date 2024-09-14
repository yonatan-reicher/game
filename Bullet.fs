module Bullet

open Maths


type Bullet =
    { Position: vec2<meter>
      Angle: float32<rad>
      Speed: float32<meter / second>
      TrailId: BulletTrail.TrailId }


let radius = 3f<m>
let distanceToBack = 8f<m>


let move (bullet: Bullet) (move: vec2<meter>) : Bullet =
    { bullet with
        Position = bullet.Position + move }


let backPosition (bullet: Bullet) : vec2<meter> =
    bullet.Position - distanceToBack * Vector.fromAngle bullet.Angle


let tick (tick: Time.Frame) (bullet: Bullet) : Bullet =
    let positionDiff = bullet.Speed * tick.Delta * Vector.fromAngle bullet.Angle
    move bullet positionDiff


let draw (bullet: Bullet) (context: Rendering.CanvasContext) =
    context.fillStyle <- Fable.Core.U3.Case1 "red"
    context.strokeStyle <- Fable.Core.U3.Case1 "red"
    context.lineWidth <- float (2f * radius)
    context.beginPath ()
    context.arc (float bullet.Position.X, float bullet.Position.Y, float radius, 0.0, 2.0 * System.Math.PI)
    context.fill ()
    context.beginPath ()
    context.moveTo (float bullet.Position.X, float bullet.Position.Y)

    context.lineTo (
        float <| bullet.Position.X - distanceToBack * cos bullet.Angle,
        float <| bullet.Position.Y - distanceToBack * sin bullet.Angle
    )

    context.stroke ()

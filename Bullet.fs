module Bullet

open Maths
open Game


let radius = 0.2f<m>
let distanceToBack = 0.4f<m>
let defaultSpeed = 60f<m / s>


let private move (bullet: Bullet) (move: vec2<meter>) : Bullet =
    { bullet with
        Position = bullet.Position + move }


/// Spawns a bullet at the given position and angle with default parameters.
let spawnAt pos angle : Bullet =
    { Position = pos
      Angle = angle
      Speed = defaultSpeed
      State = Moving
      Trail = BulletTrail.emptyTrail }


/// Makes the bullet not hit anything until exiting it's current collision.
/// NOTE: This actually just makes it wait until it has no collision before
/// hitting
/// TODO: Address the note above!
let ignoreCurrentCollision: Bullet -> Bullet =
    fun b -> { b with State = ExitingCollision }


let backPosition (bullet: Bullet) : vec2<meter> =
    bullet.Position - distanceToBack * Vector.fromAngle bullet.Angle


let tick (tick: Time.Frame) (bullet: Bullet) : Bullet =
    let positionDiff = bullet.Speed * tick.Delta * Vector.fromAngle bullet.Angle

    move bullet positionDiff
    |> fun b ->
        { b with
            Trail = BulletTrail.tick b.Position b.Trail }


let drawBulletBody (bullet: Bullet) (context: Rendering.CanvasContext) =
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


let draw (bullet: Bullet) (context: Rendering.CanvasContext) =
    drawBulletBody bullet context
    BulletTrail.draw radius bullet.Trail context

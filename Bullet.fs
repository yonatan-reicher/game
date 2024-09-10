module Bullet

open Maths
open FSharp.Data.UnitSystems.SI.UnitNames

type Bullet = 
    { Position: vec2<meter>
      Angle: float32<rad>
      Speed: float32<meter / second> }

let move (bullet: Bullet) (move: vec2<meter>): Bullet =
    { bullet with Position = bullet.Position + move }

let tick (tick: Time.FrameTime) (bullet: Bullet): Bullet =
    let positionDiff = bullet.Speed * tick.Delta * Vector.fromAngle bullet.Angle
    move bullet positionDiff

let draw (bullet: Bullet) (context: Rendering.CanvasContext) =
    context.fillStyle <- Fable.Core.U3.Case1 "red"
    context.beginPath ()
    context.arc (float bullet.Position.X, float bullet.Position.Y, 5.0, 0.0, 2.0 * System.Math.PI)
    context.fill ()

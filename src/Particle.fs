namespace Particle


open Maths
open Sprite
open Rendering


type Particle =
    { Pos: Vec2<m>
      Vel: Vec2<m / s>
      Rot: float32<rad>
      AngVel: float32<rad / s>
      SizeFunction: Particle -> float32<m>
      Lifetime: float32<s>
      TimeAlive: float32<s>
      Sprite: Sprite }

    interface Physics.Velocity<Particle> with
        member this.Pos = this.Pos
        member this.WithPos p = { this with Pos = p }
        member this.Vel = this.Vel
        member this.WithVel v = { this with Vel = v }

    interface Physics.AngularVelocity<Particle> with
        member this.Rot = this.Rot
        member this.WithRot r = { this with Rot = r }
        member this.AngVel = this.AngVel
        member this.WithAngVel v = { this with AngVel = v }

    member this.Size = this.SizeFunction this
    member this.TimeFraction: float32 = this.TimeAlive / this.Lifetime |> clamp 0f 1f


[<RequireQualifiedAccess>]
module Particle =
    let isAlive (p: Particle) : bool = p.TimeAlive < p.Lifetime

    let tick (delta: float32<s>) (p: Particle) : Particle option =
        p
        |> Physics.tickVelocity delta
        |> Physics.tickAngularVelocity delta
        |> fun p ->
            { p with
                TimeAlive = p.TimeAlive + delta }
        |> fun p -> if isAlive p then Some p else None

    let draw (p: Particle) : Draw<m, unit> =
        Sprite.draw_
            { Position = p.Pos
              Width = p.Size
              Rotation = p.Rot }
            p.Sprite

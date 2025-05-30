namespace Game

open Maths
open Util
open Draw
open Rendering



module Enemy =
    let private shootDelay = 2f<s>

    let initAt pos : Enemy =
        { Position = pos
          ShootDelay = shootDelay }


    let draw ({ Position = pos; ShootDelay=s }: Enemy) (context: Rendering.CanvasContext) =
        let r = 0.5
        context.fillStyle <- Fable.Core.U3.Case1 "red"
        context.fillRect (float pos.X - r, float pos.Y - r, 2.0 * r, 2.0 * r)

        { Value = 1f - s / shootDelay
          Color = "red"
          BackgroundColor = "gray"
          Width = 1f<m>
          Height = 0.2f<m> }
        |> Bar.draw
        |> Draw.moveBy (pos + vec2 -0.5f<m> 1f<m>)
        |> Draw.action
        <| context


    let private moveToAimAt (ft: Time.Frame) targetPos (enemy: Enemy) =
        let desiredDis = 8f<m>
        let speed = 4f<m / s>
        let dir = (targetPos - enemy.Position) |> Vector.normalize
        let desiredPosition = targetPos - dir * desiredDis
        let position = Vector.moveTo desiredPosition enemy.Position (speed * ft.Delta)
        { enemy with Position = position }


    let private makeBullet pos targetPos : Bullet =
        let bulletAngle = Vector.angle (targetPos - pos)
        Bullet.spawnAt pos bulletAngle |> Bullet.ignoreCurrentCollision


    let private shootIfCan at : Enemy -> Enemy * Bullet option =
        fun enemy ->
            if enemy.ShootDelay > 0f<s> then
                enemy, None
            else
                { enemy with ShootDelay = shootDelay }, Some(makeBullet enemy.Position at)


    let private decreaseDelay (ft: Time.Frame) enemy =
        { enemy with
            ShootDelay = enemy.ShootDelay - ft.Delta }


    let tick (ft: Time.Frame) targetPos : Enemy -> Enemy * Bullet option =
        moveToAimAt ft targetPos >> shootIfCan targetPos >> mapFst (decreaseDelay ft)

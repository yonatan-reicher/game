namespace Game

open Maths
open Util


module Enemy =
    let initAt pos : Enemy = { Position = pos; ShootDelay = 5f<s> }


    let draw ({ Position = pos }: Enemy) (context: Rendering.CanvasContext) =
        let r = 0.5
        context.fillStyle <- Fable.Core.U3.Case1 "red"
        context.fillRect (float pos.X - r, float pos.Y - r, 2.0 * r, 2.0 * r)


    let private moveToAimAt (ft: Time.Frame) targetPos (enemy: Enemy) =
        let desiredDis = 4f<m>
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
                { enemy with ShootDelay = 5f<s> }, Some(makeBullet enemy.Position at)


    let private decreaseDelay (ft: Time.Frame) enemy =
        { enemy with
            ShootDelay = enemy.ShootDelay - ft.Delta }


    let tick (ft: Time.Frame) targetPos : Enemy -> Enemy * Bullet option =
        moveToAimAt ft targetPos >> shootIfCan targetPos >> mapFst (decreaseDelay ft)

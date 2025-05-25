/// This module is responsible for the player's shooting when the mouse is
/// held, and managing the cool-down between shots and whatever
module Game.ShootingState

open Maths
open Util


type SS = ShootingState


let fullWait : ShootingState = SS.Recovering 1f<s>


/// This function returns the result of triggering a shot. Returns some
/// bullet to shoot if the shot is successful (not in cool-down)
let shoot playerPos targetLocation =
    function
    | SS.Idle ->
        let angle = Vector.angle (targetLocation - playerPos)
        Some(Bullet.spawnAt playerPos angle, fullWait)
    | SS.Recovering _ -> None


let tick (ft: Time.Frame) : SS -> SS =
    function
    | SS.Idle -> SS.Idle
    | SS.Recovering timeLeft ->
        let timeLeft = timeLeft - ft.Delta

        match timeLeft with
        | Positive -> SS.Recovering timeLeft
        | _ -> SS.Idle

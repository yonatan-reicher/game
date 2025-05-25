module Game.Chest

open Maths
open Sprite


let width = 1.0f<m>

module Sprites =
    let closed = Sprite.fromUrl "assets/chest-closed.png"
    let opened = Sprite.fromUrl "assets/chest-opened.png"


let draw (chest: Chest) =
    let sprite = if chest.Opened then Sprites.opened else Sprites.closed

    Sprite.draw
        { Position = chest.Position
          Width = width
          Rotation = 0f<rad> }
        sprite


let setOpened (chest: Chest) = { chest with Opened = true }


let circle (chest: Chest) : Circle<m> =
    { Center = chest.Position
      Radius = width * 0.5f }


let chip (chest: Chest) = chest.Chip

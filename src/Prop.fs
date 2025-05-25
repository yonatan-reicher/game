module Prop

open Game
open Sprite


let draw (prop: Prop) =
    Sprite.draw
        { Position = prop.Position
          Width = prop.Width
          Rotation = prop.Rotation }
        prop.Sprite

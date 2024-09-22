module Prop

open Game
open Sprite


let draw (prop: Prop) =
    Sprite.draw prop.Position prop.Width prop.Rotation prop.Sprite

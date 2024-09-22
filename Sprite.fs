namespace Sprite
// This namespace is all about sprites! Basically, images you can draw in
// various ways.

open Maths


type Sprite = private Sprite of Browser.Types.HTMLImageElement


module Sprite =
    let fromUrl url =
        let img = Browser.Dom.Image.Create()
        img.src <- url
        Sprite img


    let element (Sprite img) = img


    let size (Sprite img) : vec2<px> =
        vec2 (1f<px> * float32 img.naturalWidth) (1f<px> * float32 img.naturalHeight)


    let heightOverWidth (Sprite img) =
        float32 img.naturalHeight / float32 img.naturalWidth


    let draw (position: vec2<m>)
             (width: float32<m>)
             (rotation: float32<rad>)
             (sprite: Sprite) =       
        Rendering.movedTo
            (position / 1f<m>)
            (Rendering.rotatedAround Vector.zero rotation (fun context ->
                let loaded = (element sprite).complete
                if loaded then
                    let w = float width
                    let h = w * float (heightOverWidth sprite)

                    let img = Fable.Core.U3.Case1(element sprite)
                    context.drawImage (img, -0.5 * w, -0.5 * h, w, h)))

namespace Sprite
// This namespace is all about sprites! Basically, images you can draw in
// various ways.

open Maths


type Sprite = private Sprite of Browser.Types.HTMLImageElement * flippedX: bool


module Sprite =
    let fromUrl url =
        let img = Browser.Dom.Image.Create()
        img.src <- url
        Sprite(img, false)


    let flipX (Sprite(img, alreadyFlipped)) = Sprite(img, not alreadyFlipped)


    let element (Sprite(img, _)) = img


    let size sprite : vec2<px> =
        let img = element sprite
        vec2 (1f<px> * float32 img.naturalWidth) (1f<px> * float32 img.naturalHeight)


    let heightOverWidth sprite =
        let img = element sprite
        float32 img.naturalHeight / float32 img.naturalWidth


    let isFlipped (Sprite(_, x)) = x


    /// Draws the image element of the sprite without rotation, positioning or
    /// flipping. Does do scaling.
    let private draw' width sprite =
        (fun (context: Rendering.CanvasContext) ->
            let loaded = (element sprite).complete

            if loaded then
                let w = float width
                let h = w * float (heightOverWidth sprite)

                let img = Fable.Core.U3.Case1(element sprite)
                context.drawImage (img, -0.5 * w, -0.5 * h, w, h))


    let draw (position: vec2<m>) (width: float32<m>) (rotation: float32<rad>) (sprite: Sprite) =
        draw' width sprite
        |> if isFlipped sprite then Rendering.flippedX else id
        |> Rendering.flippedY
        |> Rendering.rotatedAround Vector.zero rotation
        |> Rendering.movedTo (position / 1f<m>)

module Prop

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


type Prop =
    { Position: vec2<meter>
      Width: float32<meter>
      Rotation: float32<rad>
      Sprite: Sprite }


let draw (prop: Prop) =
    Rendering.movedTo
        (prop.Position / 1f<m>)
        (Rendering.rotatedAround Vector.zero prop.Rotation (fun context ->
            let loaded = (Sprite.element prop.Sprite).complete
            printfn "Loaded: %A" prop

            if loaded then
                let w = float prop.Width
                let h = w * float (Sprite.heightOverWidth prop.Sprite)

                let img = Fable.Core.U3.Case1(Sprite.element prop.Sprite)
                context.drawImage (img, -0.5 * w, -0.5 * h, w, h)))

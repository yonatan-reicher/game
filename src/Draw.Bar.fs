namespace Draw


open Rendering
open Rendering.Draw.CE


[<Struct>]
type Bar<[<Measure>] 'm> =
    { Value: float32
      Color: string
      BackgroundColor: string
      Width: float32<'m>
      Height: float32<'m> }


[<RequireQualifiedAccess>]
module Bar =
    let draw (bar: Bar<'m>) : Draw<'m, unit> =
        draw {
            do! Draw.rect (0f<_>, 0f<_>, bar.Width, bar.Height, bar.BackgroundColor)
            do! Draw.rect (0f<_>, 0f<_>, bar.Width * bar.Value, bar.Height, bar.Color)
        }

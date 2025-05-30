namespace Draw


open Rendering
open FSharpPlus


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
        Draw.rect (0f<_>, 0f<_>, bar.Width, bar.Height, bar.BackgroundColor)
        >>= fun () ->
            Draw.rect (0f<_>, 0f<_>, bar.Width * bar.Value, bar.Height, bar.Color)
        // It seems that the `monad` builder is not supported with Fable...
        // (It does not compile)
        // monad {
        //     do! Draw.rect (0f<_>, 0f<_>, bar.Width, bar.Height, bar.BackgroundColor)
        //     do! Draw.rect (0f<_>, 0f<_>, bar.Width * bar.Value, bar.Height, bar.Color)
        // }

namespace Rendering.Draw


open Rendering


type Builder() =
    member inline _.Return x : Draw<'u, 'a> = ret x
    member inline _.ReturnFrom x : Draw<'u, 'a> = x
    member inline _.Zero () : Draw<'u, unit> = ret ()
    member inline _.Bind(x: Draw<'u, 'a>, f: 'a -> Draw<'u, 'b>) : Draw<'u, 'b> =
        x >>= f
    // Draw objects are already delayed!
    member inline _.Delay(f: unit -> Draw<'u, 'a>) : Draw<'u, 'a> = f ()


module CE =
    let draw = Builder()

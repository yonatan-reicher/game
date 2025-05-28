namespace rec Rendering


open Fable.Core
open Rendering
open Browser.Types


/// Text that can be rendered on the screen.
type Text =
    { Text: string
      Font: string
      TextAlign: Text.Align
      Size: int
      Color: U3<string, CanvasGradient, CanvasPattern>
      MaxWidth: float option }


module Text =

    [<RequireQualifiedAccess; Struct>]
    type Align =
        | Start
        | End
        | Left
        | Right
        | Center

        override this.ToString() =
            match this with
            | Start -> "start"
            | End -> "end"
            | Left -> "left"
            | Right -> "right"
            | Center -> "center"

    let text (words: string) : Text =
        { Text = words
          Font = "monospace"
          TextAlign = Align.Left
          Size = 16
          Color = U3.Case1 "black"
          MaxWidth = None }


    let inline withFont font (text: Text) = { text with Font = font }


    let withAlignment alignment (text: Text) = { text with TextAlign = alignment }


    let inline withSize size (text: Text) = { text with Size = size }


    let inline withColor color (text: Text) = { text with Color = color }


    let inline withMaxWidth maxWidth (text: Text) = { text with MaxWidth = Some maxWidth }


    let inline draw (text: Text) (context: CanvasContext) =
        context.font <- sprintf "%dpx %s" text.Size text.Font
        context.textAlign <- string text.TextAlign
        context.fillStyle <- text.Color

        flippedY
            (fun context ->
                match text.MaxWidth with
                | None -> context.fillText (text.Text, 0.0, 0.0)
                | Some maxWidth -> context.fillText (text.Text, 0.0, 0.0, maxWidth))
            context

module Input

open Browser

open Maths

[<Struct>]
type Key =
    | AltLeft
    | AltRight
    | ArrowDown
    | ArrowLeft
    | ArrowRight
    | ArrowUp
    | Backquote
    | Backslash
    | Backspace
    | BracketLeft
    | BracketRight
    | CapsLock
    | Comma
    | ControlLeft
    | ControlRight
    | Delete
    | Digit0
    | Digit1
    | Digit2
    | Digit3
    | Digit4
    | Digit5
    | Digit6
    | Digit7
    | Digit8
    | Digit9
    | End
    | Enter
    | Equal
    | Escape
    | F1
    | F10
    | F11
    | F12
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | Home
    | Insert
    | KeyA
    | KeyB
    | KeyC
    | KeyD
    | KeyE
    | KeyF
    | KeyG
    | KeyH
    | KeyI
    | KeyJ
    | KeyK
    | KeyL
    | KeyM
    | KeyN
    | KeyO
    | KeyP
    | KeyQ
    | KeyR
    | KeyS
    | KeyT
    | KeyU
    | KeyV
    | KeyW
    | KeyX
    | KeyY
    | KeyZ
    | Minus
    | NumLock
    | PageDown
    | PageUp
    | Pause
    | Period
    | PrintScreen
    | Quote
    | ScrollLock
    | Semicolon
    | ShiftLeft
    | ShiftRight
    | Slash
    | Space
    | Tab

let keyToInt =
    function
    | AltLeft -> 0
    | AltRight -> 1
    | ArrowDown -> 2
    | ArrowLeft -> 3
    | ArrowRight -> 4
    | ArrowUp -> 5
    | Backquote -> 6
    | Backslash -> 7
    | Backspace -> 8
    | BracketLeft -> 9
    | BracketRight -> 10
    | CapsLock -> 11
    | Comma -> 12
    | ControlLeft -> 13
    | ControlRight -> 14
    | Delete -> 15
    | Digit0 -> 16
    | Digit1 -> 17
    | Digit2 -> 18
    | Digit3 -> 19
    | Digit4 -> 20
    | Digit5 -> 21
    | Digit6 -> 22
    | Digit7 -> 23
    | Digit8 -> 24
    | Digit9 -> 25
    | End -> 26
    | Enter -> 27
    | Equal -> 28
    | Escape -> 29
    | F1 -> 30
    | F10 -> 31
    | F11 -> 32
    | F12 -> 33
    | F2 -> 34
    | F3 -> 35
    | F4 -> 36
    | F5 -> 37
    | F6 -> 38
    | F7 -> 39
    | F8 -> 40
    | F9 -> 41
    | Home -> 42
    | Insert -> 43
    | KeyA -> 44
    | KeyB -> 45
    | KeyC -> 46
    | KeyD -> 47
    | KeyE -> 48
    | KeyF -> 49
    | KeyG -> 50
    | KeyH -> 51
    | KeyI -> 52
    | KeyJ -> 53
    | KeyK -> 54
    | KeyL -> 55
    | KeyM -> 56
    | KeyN -> 57
    | KeyO -> 58
    | KeyP -> 59
    | KeyQ -> 60
    | KeyR -> 61
    | KeyS -> 62
    | KeyT -> 63
    | KeyU -> 64
    | KeyV -> 65
    | KeyW -> 66
    | KeyX -> 67
    | KeyY -> 68
    | KeyZ -> 69
    | Minus -> 70
    | NumLock -> 71
    | PageDown -> 88
    | PageUp -> 89
    | Pause -> 90
    | Period -> 91
    | PrintScreen -> 92
    | Quote -> 93
    | ScrollLock -> 94
    | Semicolon -> 95
    | ShiftLeft -> 96
    | ShiftRight -> 97
    | Slash -> 98
    | Space -> 99
    | Tab -> 100

let keyToString =
    function
    | AltLeft -> "AltLeft"
    | AltRight -> "AltRight"
    | ArrowDown -> "ArrowDown"
    | ArrowLeft -> "ArrowLeft"
    | ArrowRight -> "ArrowRight"
    | ArrowUp -> "ArrowUp"
    | Backquote -> "Backquote"
    | Backslash -> "Backslash"
    | Backspace -> "Backspace"
    | BracketLeft -> "BracketLeft"
    | BracketRight -> "BracketRight"
    | CapsLock -> "CapsLock"
    | Comma -> "Comma"
    | ControlLeft -> "ControlLeft"
    | ControlRight -> "ControlRight"
    | Delete -> "Delete"
    | Digit0 -> "Digit0"
    | Digit1 -> "Digit1"
    | Digit2 -> "Digit2"
    | Digit3 -> "Digit3"
    | Digit4 -> "Digit4"
    | Digit5 -> "Digit5"
    | Digit6 -> "Digit6"
    | Digit7 -> "Digit7"
    | Digit8 -> "Digit8"
    | Digit9 -> "Digit9"
    | End -> "End"
    | Enter -> "Enter"
    | Equal -> "Equal"
    | Escape -> "Escape"
    | F1 -> "F1"
    | F10 -> "F10"
    | F11 -> "F11"
    | F12 -> "F12"
    | F2 -> "F2"
    | F3 -> "F3"
    | F4 -> "F4"
    | F5 -> "F5"
    | F6 -> "F6"
    | F7 -> "F7"
    | F8 -> "F8"
    | F9 -> "F9"
    | Home -> "Home"
    | Insert -> "Insert"
    | KeyA -> "KeyA"
    | KeyB -> "KeyB"
    | KeyC -> "KeyC"
    | KeyD -> "KeyD"
    | KeyE -> "KeyE"
    | KeyF -> "KeyF"
    | KeyG -> "KeyG"
    | KeyH -> "KeyH"
    | KeyI -> "KeyI"
    | KeyJ -> "KeyJ"
    | KeyK -> "KeyK"
    | KeyL -> "KeyL"
    | KeyM -> "KeyM"
    | KeyN -> "KeyN"
    | KeyO -> "KeyO"
    | KeyP -> "KeyP"
    | KeyQ -> "KeyQ"
    | KeyR -> "KeyR"
    | KeyS -> "KeyS"
    | KeyT -> "KeyT"
    | KeyU -> "KeyU"
    | KeyV -> "KeyV"
    | KeyW -> "KeyW"
    | KeyX -> "KeyX"
    | KeyY -> "KeyY"
    | KeyZ -> "KeyZ"
    | Minus -> "Minus"
    | NumLock -> "NumLock"
    | PageDown -> "PageDown"
    | PageUp -> "PageUp"
    | Pause -> "Pause"
    | Period -> "Period"
    | PrintScreen -> "PrintScreen"
    | Quote -> "Quote"
    | ScrollLock -> "ScrollLock"
    | Semicolon -> "Semicolon"
    | ShiftLeft -> "ShiftLeft"
    | ShiftRight -> "ShiftRight"
    | Slash -> "Slash"
    | Space -> "Space"
    | Tab -> "Tab"

let stringToKey =
    function
    | "AltLeft" -> Some AltLeft
    | "AltRight" -> Some AltRight
    | "ArrowDown" -> Some ArrowDown
    | "ArrowLeft" -> Some ArrowLeft
    | "ArrowRight" -> Some ArrowRight
    | "ArrowUp" -> Some ArrowUp
    | "Backquote" -> Some Backquote
    | "Backslash" -> Some Backslash
    | "Backspace" -> Some Backspace
    | "BracketLeft" -> Some BracketLeft
    | "BracketRight" -> Some BracketRight
    | "CapsLock" -> Some CapsLock
    | "Comma" -> Some Comma
    | "ControlLeft" -> Some ControlLeft
    | "ControlRight" -> Some ControlRight
    | "Delete" -> Some Delete
    | "Digit0" -> Some Digit0
    | "Digit1" -> Some Digit1
    | "Digit2" -> Some Digit2
    | "Digit3" -> Some Digit3
    | "Digit4" -> Some Digit4
    | "Digit5" -> Some Digit5
    | "Digit6" -> Some Digit6
    | "Digit7" -> Some Digit7
    | "Digit8" -> Some Digit8
    | "Digit9" -> Some Digit9
    | "End" -> Some End
    | "Enter" -> Some Enter
    | "Equal" -> Some Equal
    | "Escape" -> Some Escape
    | "F1" -> Some F1
    | "F10" -> Some F10
    | "F11" -> Some F11
    | "F12" -> Some F12
    | "F2" -> Some F2
    | "F3" -> Some F3
    | "F4" -> Some F4
    | "F5" -> Some F5
    | "F6" -> Some F6
    | "F7" -> Some F7
    | "F8" -> Some F8
    | "F9" -> Some F9
    | "Home" -> Some Home
    | "Insert" -> Some Insert
    | "KeyA" -> Some KeyA
    | "KeyB" -> Some KeyB
    | "KeyC" -> Some KeyC
    | "KeyD" -> Some KeyD
    | "KeyE" -> Some KeyE
    | "KeyF" -> Some KeyF
    | "KeyG" -> Some KeyG
    | "KeyH" -> Some KeyH
    | "KeyI" -> Some KeyI
    | "KeyJ" -> Some KeyJ
    | "KeyK" -> Some KeyK
    | "KeyL" -> Some KeyL
    | "KeyM" -> Some KeyM
    | "KeyN" -> Some KeyN
    | "KeyO" -> Some KeyO
    | "KeyP" -> Some KeyP
    | "KeyQ" -> Some KeyQ
    | "KeyR" -> Some KeyR
    | "KeyS" -> Some KeyS
    | "KeyT" -> Some KeyT
    | "KeyU" -> Some KeyU
    | "KeyV" -> Some KeyV
    | "KeyW" -> Some KeyW
    | "KeyX" -> Some KeyX
    | "KeyY" -> Some KeyY
    | "KeyZ" -> Some KeyZ
    | "Minus" -> Some Minus
    | "NumLock" -> Some NumLock
    | "PageDown" -> Some PageDown
    | "PageUp" -> Some PageUp
    | "Pause" -> Some Pause
    | "Period" -> Some Period
    | "PrintScreen" -> Some PrintScreen
    | "Quote" -> Some Quote
    | "ScrollLock" -> Some ScrollLock
    | "Semicolon" -> Some Semicolon
    | "ShiftLeft" -> Some ShiftLeft
    | "ShiftRight" -> Some ShiftRight
    | "Slash" -> Some Slash
    | "Space" -> Some Space
    | "Tab" -> Some Tab
    | _ -> None

// Evil mutable, global state!
let private keys = Array.zeroCreate 256
let mutable private mouse: Maths.vec2 = { X = 0f; Y = 0f }

// Set up key listeners
window.onkeydown <-
    fun e ->
        e.code
        |> stringToKey
        |> Option.map keyToInt
        |> Option.iter (fun i -> keys.[i] <- true)

window.onkeyup <-
    fun e ->
        e.code
        |> stringToKey
        |> Option.map keyToInt
        |> Option.iter (fun i -> keys.[i] <- false)

window.onmousemove <-
    fun e ->
        mouse <-
            { X = float32 e.clientX
              Y = float32 e.clientY }

let isKeyDown (key: Key) = keys.[keyToInt key]
let getMousePosition () = mouse


type InputConfig<'state> =
    { State: 'state ref
      OnMouseDown: Maths.vec2<Maths.px> -> 'state -> 'state
      OnMouseUp: Maths.vec2<Maths.px> -> 'state -> 'state
      OnKeyDown: Key -> 'state -> 'state
      OnKeyUp: Key -> 'state -> 'state }


let setup
    ({ State = state
       OnMouseDown = onMouseDown
       OnMouseUp = onMouseUp
       OnKeyUp = onKeyUp
       OnKeyDown = onKeyDown }: InputConfig<'state>)
    =
    window.onmousedown <-
        fun e ->
            let pos =
                { X = 1f<Maths.px> * float32 e.clientX
                  Y = 1f<Maths.px> * float32 e.clientY }

            state.Value <- onMouseDown pos state.Value

    window.onmouseup <-
        fun e ->
            let pos =
                { X = 1f<Maths.px> * float32 e.clientX
                  Y = 1f<Maths.px> * float32 e.clientY }

            state.Value <- onMouseUp pos state.Value

    let old = window.onkeyup

    window.onkeyup <-
        fun e ->
            old e

            e.code
            |> stringToKey
            |> Option.iter (fun key -> state.Value <- onKeyUp key state.Value)

    let old = window.onkeydown

    window.onkeydown <-
        fun e ->
            old e

            e.code
            |> stringToKey
            |> Option.iter (fun key -> state.Value <- onKeyDown key state.Value)

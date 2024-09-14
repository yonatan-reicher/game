module Time

open Browser
open FSharp.Data.UnitSystems.SI.UnitSymbols

let private Float32WithMeasure = LanguagePrimitives.Float32WithMeasure

[<Measure>]
type frame

[<Measure>]
type ms

let msInS: float32<ms / s> = 1000.0f<ms / s>
let sInMs: float32<s / ms> = 1.f / Float32WithMeasure(float32 msInS)

type Frame =
    {
        /// This is the amount of time to simulate between each simulation step.
        /// Note: This is *not* the actual time between frames.
        Delta: float32<s>
        /// This is the amout of time that would have passed between frames if
        /// the game was running at perfect fps. 
        /// UnscaledDelta = Delta / Time.timeScale
        UnscaledDelta: float32<s>
    }

type TimeConfig<'state> =
    { FrameRate: float32<frame / s>
      Tick: 'state -> Frame -> 'state
      State: 'state ref }


/// Is the game paused?
let paused = ref false
/// Is the game paused?
let timeScale = ref 1.0f
/// The amount of time the game has ran, in simulation time.
let private elapsed = ref 0.0f<s>
/// The amount of frames simulated.
let frames = ref 0
/// The amount of time the game has ran, in simulation time.
let getElapsed () = elapsed.Value


window.onblur <- fun _ -> paused.Value <- true
window.onfocus <- fun _ -> paused.Value <- false


let setup
    ({ FrameRate = frameRate
       Tick = tick
       State = state }: _ TimeConfig)
    =

    let unscaledDelta = 1.0f / frameRate

    let onInterval (_) =

        // This is close, but not quite accurate because of javascript's
        // clock. Whatever.

        frames.Value <- frames.Value + 1

        if not paused.Value then
            let frameData: Frame = { Delta = unscaledDelta * timeScale.Value * 1f<frame>
                                     UnscaledDelta = unscaledDelta * 1f<frame> }
            state.Value <- tick state.Value frameData
            elapsed.Value <- elapsed.Value + unscaledDelta * 1f<frame> * timeScale.Value

    window.setInterval (onInterval, int (msInS / frameRate)) |> ignore

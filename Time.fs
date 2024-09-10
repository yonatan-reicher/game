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

type FrameTime =
    {
        /// This is the amount of time to simulate between each simulation step.
        /// Note: This is *not* the actual time between frames.
        Delta: float32<s>
    }

type TimeOptions<'state> =
    { FrameRate: float32<frame / s>
      Tick: 'state -> FrameTime -> 'state
      State: 'state ref }

let paused = ref false
/// The amount of time the game has ran, in simulation time.
let private elapsed = ref 0.0f<s>
/// The amount of time the game has ran, in simulation time.
let getElapsed () = elapsed.Value

window.onblur <- fun _ -> paused.Value <- true
window.onfocus <- fun _ -> paused.Value <- false

let setup
    ({ FrameRate = frameRate
       Tick = tick
       State = state }: _ TimeOptions)
    =
    // Set up tick loop
    let mutable frames = 0
    let mutable startTime = System.DateTime.Now
    let mutable catchingUp = false

    let delta = 1.0f / frameRate

    let onInterval (_) =

        // This is close, but not quite accurate because of javascript's
        // clock.
        // state.Value <- tick state.Value { Delta = delta }

        if catchingUp then ()
        else
            catchingUp <- true
            let totalTime = float32 (System.DateTime.Now - startTime).TotalSeconds * 1f<s>

            let mutable i = 0
            while 1f<frame> * float32 frames - frameRate * totalTime < 0f<frame> do
                frames <- frames + 1
                if not paused.Value then 
                    state.Value <- tick state.Value { Delta = 1f<frame> * delta }
                    elapsed.Value <- elapsed.Value + delta * 1f<frame>
                else
                    //printfn "Paused"
                    ()
                i <- i + 1

            //printfn "Caught up %d frames" i

            catchingUp <- false

    window.setInterval (onInterval, int (msInS / frameRate)) |> ignore

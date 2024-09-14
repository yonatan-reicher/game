module BulletTrail

open Maths

// Representation:
// A list of bullet positions revere order.
// (Reverse order so that adding is O(1)).
type Trail = vec2<meter> list

[<Struct>]
type TrailId = private TrailId of int

[<Struct>]
type State = private State of Map<TrailId, Trail>


let private nextId = ref 0


let newTrail () : TrailId =
    let ret = TrailId nextId.Value
    nextId.Value <- nextId.Value + 1
    ret


let empty: State = State Map.empty


[<Struct>]
type TickInput =
    { PlayerPosition: vec2<meter>
      State: State
      BulletPositions: (TrailId * vec2<meter>) seq }


let tick
    ({ PlayerPosition = _
       State = State trails
       BulletPositions = bullets }: TickInput)
    : State =
    bullets
    |> Seq.fold
        (fun state (id, bulletPos) ->
            let trail = Map.tryFind id trails |> Option.defaultValue []
            let newTrail = bulletPos :: trail |> List.truncate 3
            Map.add id newTrail state)
        trails
    |> State


let remove (id: TrailId) (State trails) : State = State(Map.remove id trails)


let draw (radius: float32<m>) (State trails) (context: Rendering.CanvasContext) =
    context.fillStyle <- Fable.Core.U3.Case1 "red"
    context.strokeStyle <- Fable.Core.U3.Case1 "red"
    context.lineWidth <- float (2f * radius)

    for points in trails.Values do
        let pairs = Seq.zip points (Seq.skip 1 points)

        context.beginPath ()

        for (a, b) in pairs do
            context.moveTo (float a.X, float a.Y)
            context.lineTo (float b.X, float b.Y)

        context.closePath ()
        context.stroke ()

        (*
        for { X = x; Y = y } in points do
            context.ellipse (float x, float y, 5, 5, 0, 0, 2. * System.Math.PI)
            context.fill()
            *)

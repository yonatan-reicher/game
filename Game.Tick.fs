module Game.Tick

open Maths
open Util


let constrainCameraToPlayer (state: State) =
    { state with
        Camera =
            { state.Camera with
                Position = state.Player.Position } }


let doTrails (state: State) : State =
    { state with
        BulletTrail =
            BulletTrail.tick
                { PlayerPosition = state.Player.Position
                  State = state.BulletTrail
                  BulletPositions = seq { for b in state.Bullets -> (b.TrailId, Bullet.backPosition b) } } }


type Hits =
    { Hit: (Bullet * Enemy) list
      EnemiesNotHit: Enemy list
      BulletsNotHit: Bullet list }


let changeExitingToMoving: Bullet -> Bullet =
    function
    | { State = ExitingCollision } as b -> { b with State = Moving }
    | b -> b


let doBulletHits (ft: Time.Frame) : State -> State =
    let collides (bullet: Bullet) (enemy: Enemy) =
        let distance = Vector.length (bullet.Position - enemy.Position)
        // distance < Bullet.radius + Enemy.radius
        distance < Bullet.radius + 0.6f<m>

    let getHits (state: State) : Hits =
        // Works by iterating over all enemies, and marking bullets that hit them.
        List.fold
            (fun hits enemy ->
                List.tryFindAndRest (fun b -> collides b enemy) hits.BulletsNotHit
                |> function
                    | Some(bullet, others) ->
                        { hits with
                            Hit = (bullet, enemy) :: hits.Hit
                            BulletsNotHit = others }
                    | None ->
                        { hits with
                            EnemiesNotHit = enemy :: hits.EnemiesNotHit })
            { Hit = []
              EnemiesNotHit = []
              BulletsNotHit = state.Bullets }
            state.Enemies


    let shouldBeIgnored: Bullet * Enemy -> bool =
        function
        | ({ State = Moving }, _) -> false
        | ({ State = ExitingCollision }, _) -> true

    fun state ->
        let hits = getHits state

        /// These are the bullets that will be next frame.
        let bullets =
            // The bullets that did not hit, and the bullets that did hit but
            // should be ignored.
            hits.BulletsNotHit
            |> List.map changeExitingToMoving
            |> (@) (List.filter shouldBeIgnored hits.Hit |> List.map fst)

        let enemies =
            hits.EnemiesNotHit @ (List.filter shouldBeIgnored hits.Hit |> List.map snd)

        { state with
            Bullets = bullets
            Enemies = enemies }


let tick (state: State) (ft: Time.Frame) =
    List.map (Enemy.tick ft state.Player.Position) state.Enemies
    |> List.unzip
    |> mapSnd (List.choose id)
    |> fun (enemies, newBullets) ->
        { state with
            Player = Player.tick state.Player ft
            Bullets = List.map (Bullet.tick ft) state.Bullets @ newBullets
            Enemies = enemies }
    |> doBulletHits ft
    |> doTrails
// |> constrainCameraToPlayer
//  |> fun s -> { s with Camera = { s.Camera with Width = Time.getElapsed() * 300f<m/s> + float32 (getCanvas ()).width * 1f<m> } }
// |> fun s -> { s with Camera = { s.Camera with Rotation = s.Camera.Rotation + 0.001f<rad> } }

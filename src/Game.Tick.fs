module Game.Tick

open Maths
open Util

(*
let constrainCameraToPlayer (state: Level) =
    { state with
        Camera =
            { state.Camera with
                Position = state.Player.Position } }
                *)


let changeExitingToMoving: Bullet -> Bullet =
    function
    | { State = ExitingCollision } as b -> { b with State = Moving }
    | b -> b


module Collision =

    type private CollisionObject =
        { Position: vec2<m>
          Radius: float32<m> }


    let private collisionObjectFromEnemy (enemy: Enemy) =
        { Position = enemy.Position
          // TODO: Radius = Enemy.radius }
          Radius = 0.6f<m> }


    let private collisionObjectFromPlayer (player: Player) =
        { Position = player.Position
          Radius = Player.radius }


    type private Hits =
        { EnemiesHit: (Bullet * Enemy) list
          PlayerHit: Bullet option
          EnemiesNotHit: Enemy list
          BulletsHitButIgnored: Bullet list
          BulletsNotHit: Bullet list }


    type private HitDetectionState =
        { EnemiesLeft: Enemy list
          BulletsLeft: Bullet list
          Hits: Hits }


    let private collides (bullet: Bullet) (something: CollisionObject) =
        let distanceSqr = Vector.sqrLength (bullet.Position - something.Position)
        let sqr x = x * x
        // distance < Bullet.radius + Enemy.radius
        distanceSqr < sqr (Bullet.radius + something.Radius)


    let shouldBeIgnored: Bullet -> bool =
        function
        | { State = Moving } -> false
        | { State = ExitingCollision } -> true


    let private initialHitDetectionState (state: Level) : HitDetectionState =
        { EnemiesLeft = state.Enemies
          BulletsLeft = state.Bullets
          Hits =
            { EnemiesHit = []
              PlayerHit = None
              EnemiesNotHit = []
              BulletsHitButIgnored = []
              BulletsNotHit = [] } }


    let private setEnemiesLeft enemies (state: HitDetectionState) = { state with EnemiesLeft = enemies }


    let private mapHits f (state: HitDetectionState) = { state with Hits = f state.Hits }


    let private markHit bullet enemy (state: HitDetectionState) =
        { state with
            Hits =
                { state.Hits with
                    EnemiesHit = (bullet, enemy) :: state.Hits.EnemiesHit } }


    let private markNotHit enemy (state: HitDetectionState) =
        { state with
            Hits =
                { state.Hits with
                    EnemiesNotHit = enemy :: state.Hits.EnemiesNotHit } }


    let private popCollidingWith something (state: HitDetectionState) : Bullet option * HitDetectionState =
        List.tryFindAndRest (fun b -> not (shouldBeIgnored b) && collides b something) state.BulletsLeft
        |> function
            | None -> None, state
            | Some(bullet, others) -> Some bullet, { state with BulletsLeft = others }


    let rec private iterateEnemies (state: HitDetectionState) : HitDetectionState =
        match state.EnemiesLeft with
        | [] -> state
        | enemy :: otherEnemies ->
            state
            |> setEnemiesLeft otherEnemies
            |> popCollidingWith (collisionObjectFromEnemy enemy)
            |> function
                | None, state -> markNotHit enemy state
                | Some bullet, state -> markHit bullet enemy state
            |> iterateEnemies


    let private doPlayer (player: Player) (state: HitDetectionState) : HitDetectionState =
        state
        |> popCollidingWith (collisionObjectFromPlayer player)
        |> function
            | None, state -> state
            | Some bullet, state -> mapHits (fun h -> { h with PlayerHit = Some bullet }) state


    let private allCollisionObjects (state: Level) =
        collisionObjectFromPlayer state.Player
        :: List.map collisionObjectFromEnemy state.Enemies


    let private doIgnoredBullets collisionObjects (state: HitDetectionState) =
        state.BulletsLeft
        |> List.partition (fun x -> List.exists (collides x) collisionObjects)
        |> fun (ignoredCollisions, notColliding) ->
            assert List.forall shouldBeIgnored ignoredCollisions
            assert List.forall (not << shouldBeIgnored) notColliding

            state
            |> mapHits (fun h ->
                { h with
                    BulletsHitButIgnored = ignoredCollisions
                    BulletsNotHit = notColliding })
            |> fun s -> { s with BulletsLeft = [] }


    let private finish (state: HitDetectionState) : Hits = state.Hits


    let private getHits (state: Level) =
        initialHitDetectionState state
        |> iterateEnemies
        |> fun s ->
            assert s.EnemiesLeft.IsEmpty
            s
        |> doPlayer state.Player
        |> doIgnoredBullets (allCollisionObjects state)
        |> finish


    let private applyHits (hits: Hits) (state: Level) : State =
        // TODO: Do something with the enemies that were hit.

        match hits.PlayerHit with
        | Some _ -> GameOver state
        | None ->
            let bullets =
                (hits.BulletsNotHit |> List.map changeExitingToMoving)
                @ hits.BulletsHitButIgnored

            Level
                { state with
                    Bullets = bullets
                    Enemies = hits.EnemiesNotHit
                    Player = state.Player }


    let doBulletHits (_ft: Time.Frame) (state: Level) : State = applyHits (getHits state) state


let private playerTick (ft: Time.Frame) (state: Level) =
    { state with
        Player = Player.tick state.Chips state.Player ft }


let private getSlowdown (state: Level) (position: vec2<m>) (direction: vec2) : float32 =
    let diff = state.Player.Position - position

    (List.exists
        (function
        | SlowdownField -> true
        | _ -> false)
        state.Chips
     && Vector.sqrLength diff < Chip.SlowdownField.radiusSqr
     && Vector.dot diff direction > 0f<_>)
    |> function
        | true -> Chip.SlowdownField.slowdown
        | false -> 1f


let private bulletsTick (ft: Time.Frame) (state: Level) =
    { state with
        Bullets =
            List.map
                (fun (bullet: Bullet) ->
                    let slowdown = getSlowdown state bullet.Position (Vector.fromAngle bullet.Angle)
                    Bullet.tick ft slowdown bullet)
                state.Bullets }


let private enemiesTick (ft: Time.Frame) (state: Level) =
    List.map (Enemy.tick ft state.Player.Position) state.Enemies
    |> List.unzip
    |> mapSnd (List.choose id)
    |> fun (enemies, newBullets) ->
        { state with
            Bullets = state.Bullets @ newBullets
            Enemies = enemies }


let private openChests (state: Level) : Level =
    let shouldBeOpened (chest: Chest) =
        not chest.Opened
        && Overlap.circleCircle (Player.circle state.Player) (Chest.circle chest)

    state.Chests
    |> List.partition shouldBeOpened
    |> fun (toBeOpened, notOpened) -> List.map Chest.setOpened toBeOpened @ notOpened, List.map Chest.chip toBeOpened
    |> fun (chests, chips) ->
        { state with
            Chests = chests
            Chips = state.Chips @ chips }


let private cameraTick (ft: Time.Frame) (state: Level) =
    { state with
        Camera = Camera.tick ft.Delta state.Camera }


let private shootingTick (ft : Time.Frame) (state: Level) =
    { state with
        ShootingState = ShootingState.tick ft state.ShootingState }



let tick' (state: Level) (ft: Time.Frame) =
    state
    |> openChests
    |> bulletsTick ft
    |> enemiesTick ft
    |> playerTick ft
    |> shootingTick ft
    |> cameraTick ft
    |> Collision.doBulletHits ft


let tick state (ft: Time.Frame) =
    match state with
    | Level l -> tick' l ft
    | GameOver _ -> state

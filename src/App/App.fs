module App

open System
open Sutil
open Sutil.DOM
open Sutil.Attr
open Browser.Types

open Constants
open Types
open Model
open Query
open Components
open Spawner
open Systems



let createWorld () =
    let worldString = """============
=.....=....=
=.....=....=
=====.=....=
=..........=
=..........=
=..........=
========.===
=......=...=
=......=...=
=..........=
============
"""

    let parseSymbol s =
        match s with
        | '=' -> Some Wall
        | '.' -> Some Floor
        | _ -> None
    
    worldString.Split(Environment.NewLine.ToCharArray())
    |> Array.map (fun rowString -> rowString.ToCharArray())
    |> Array.map (Array.map parseSymbol)
    |> Array.concat
    |> Array.filter Option.isSome
    |> Array.map Option.get




let init () : Model =
    { GameState = Start
      PlayerDirection = Noop
      PlayingSound = None
      PlayingSounds = []
      PlayMusic = false
      World = createWorld ()
      Entities = []
      EntitiesToDraw = [] }

let keyToDirection (event: KeyboardEvent) =
    match event.code with
    | "ArrowLeft" -> Left
    | "ArrowUp" -> Up
    | "ArrowRight" -> Right
    | "ArrowDown" -> Down
    | _ -> Noop

// --- MESSAGES ---
type Message =
    | StartGame
    | KeyDown of KeyboardEvent
    | ToggleMusic
    | PlaySound of string // TODO Make sounds DU?
    | SoundPlayed of Guid
    | SoundPlaying of Guid
    | SetDirection of Direction



let tick (model: Model) =
    // TODO: In the end this probably should be a fold over the systems with the model (and maybe Side Effects list of things like sounds?) as state?
    let playerAfterMovement = moveInDirection 
                                model.World 
                                (model.Entities |> hasComponents [isPosition; isBlocksMovement])
                                (model.Entities |> hasComponents [isPlayer; isPosition; isMoveByKeyboard] |> List.head)
                                model.PlayerDirection
    let m1 = {model with Entities = replaceEntity model.Entities playerAfterMovement}

    let movedRandomly = moveRandomlySystem 
                            m1.World 
                            (m1.Entities |> hasComponents [isPosition; isBlocksMovement])
                            (m1.Entities |> hasComponents [isPosition; isMoveRandomly])
    let m2 = List.fold (fun m e -> {m with Entities = replaceEntity m.Entities e}) m1 movedRandomly

    let m3 = {m2 with Entities = modifyEntitiesByPickup m2.Entities}

    { m3 with EntitiesToDraw = drawableSystem m3.Entities }

// --- MESSAGE HANDLING, MODEL UPDATES ---
let update (msg: Message) (model: Model) : Model =
    match msg with
    | StartGame ->
        { model with
            GameState = Playing
            Entities =
                [ spawnPlayer ()
                  spawnOgre 7 5
                  spawnQuestItem 10 1 "🎁"
                  spawnQuestItem 1 6 "🧸"
                  spawnQuestItem 10 8 "📗" ] }
        |> tick

    | KeyDown event ->
        if model.GameState = Playing then
            let direction = event |> keyToDirection
            tick { model with PlayerDirection = direction }
        else
            model

    | ToggleMusic -> { model with PlayMusic = not model.PlayMusic }
    
    | PlaySound soundFileName ->
        let newSound =
            { SoundId = Guid.NewGuid()
              FileName = soundFileName
              SoundState = Queued }

        { model with PlayingSounds = newSound :: model.PlayingSounds }
    
    | SoundPlayed soundId ->
        { model with PlayingSounds = List.filter (fun s -> s.SoundId <> soundId) model.PlayingSounds }
    
    | SoundPlaying soundId ->
        let setPlayingSoundToStarted s =
            if (s.SoundId = soundId) then
                { s with SoundState = Started }
            else
                s

        { model with PlayingSounds = List.map setPlayingSoundToStarted model.PlayingSounds }

    | SetDirection direction ->
        if model.GameState = Playing then
            tick { model with PlayerDirection = direction }
        else
            model

// --- VIEWS ---
let startView (dispatch) =
    Html.div [ Html.h1 "Saving Santa's"
               Html.div [ Html.p "You 🧝 need to help Santa to reclaim his basement and collect all the presents!"
                          Html.p
                              "Move with the arrow keys. Moving unto an item picks it up. Moving into an enemy attacks."
                          Html.p "Avoid 👹 that run around randomly and hit you if you come too close."
                          Html.p "Avoid 👻 that levitate to you and attack you."
                          Html.p "Pick up all presents 🎁🧸📗 before leaving the floor."
                          Html.p "Pick up 🥛 or 🍪 to regain health."
                          Html.p "If your health reaches 0 Christmas is canceled!"
                          Html.p "Reach the top to save Santa's and Christmas!"
                          Html.button [ class' "button"
                                        text "Save Santa's!"
                                        onClick (fun _ -> dispatch StartGame) [] ] ] ]

let drawWorld (world: World) =
    let floor () =
        Html.span [ Attr.className "floor"
                    Html.text " " ]

    let wall () =
        Html.span [ Attr.className "wall"
                    Html.text " " ]

    let rows = Array.chunkBySize worldWidth world

    let createCell cell =
        match cell with
        | Floor -> floor ()
        | Wall -> wall ()

    let createRow row = Html.div ([Attr.className "map-row"] @ (row |> Array.toList |> List.map createCell))

    fragment (Array.map createRow rows)

let drawEntity (drawableEntity: DrawableEntity) : SutilElement =
    Html.div [ Html.text drawableEntity.DrawableData.Icon
               Attr.className "sprite-icon"
               style [ Css.positionAbsolute
                       Css.left (gridSize * drawableEntity.Position.X)
                       Css.top (gridSize * drawableEntity.Position.Y) ] ]

let drawHud (dispatch: Dispatch<Message>) (player: Entity) =
    // TODO: Read real values from the player Entity
    let currentHealth = 2
    let maximumHealth = 3
    let lostHealth = maximumHealth - currentHealth
    let questItemsDrawable : DrawableData list =
        (getComponent (isPlayer, getPlayer) player).Inventory |> List.map getDrawable

    let hearts = Seq.append
                    (Seq.map (fun _ -> Html.span [Html.text "❤️"]) (seq [1..currentHealth]))
                    (Seq.map (fun _ -> Html.span [Html.text "💔"]) (seq [1..lostHealth]))

    let items = List.map (fun drawable -> Html.span [Html.text drawable.Icon] ) questItemsDrawable

    let controls = List.map (fun (direction, icon) -> 
                                Html.button [   
                                    class' "button hud-button"
                                    text icon
                                    onClick (fun _ -> dispatch <| SetDirection direction) []
                                ]
                            ) 
                    [
                        (Left, "⬅️")
                        (Right, "➡️")
                        (Up, "⬆️")
                        (Down, "⬇️")
                    ]

    Html.div [ 
        class' "hud"
        Html.div [
            class' "hud-player-data"
            Html.div [fragment hearts]
            Html.div [fragment items]
        ]
        Html.div [
            class' "hud-controls"
            fragment controls
        ]
    ]

let getPlayerEntity (model: Model) : Entity =
    model.Entities |> hasComponent isPlayer |> List.head

let playView (model: IStore<Model>) (dispatch: Dispatch<Message>) =
    Html.div [ Attr.className "game-world"
               Bind.el ((model |> Store.map getWorld), drawWorld)
               Bind.each ((model |> Store.map getEntitiesToDraw), drawEntity)
               Bind.el ((model |> Store.map getPlayerEntity), (drawHud dispatch))
                ]

let noopView () = Html.h1 "NOTHING HERE YET"

let createAudioTag dispatch (sound: IObservable<Sound>) =
    Bind.el (
        sound,
        fun s ->
            let shouldPlay = (s.SoundState = Queued)

            Html.audio [ on "play" (fun _ -> s.SoundId |> SoundPlaying |> dispatch) []
                         on "ended" (fun _ -> s.SoundId |> SoundPlayed |> dispatch) []
                         Attr.autoPlay shouldPlay
                         Attr.src ("sound/" + s.FileName) ]
    )

let view () =
    let model, dispatch =
        () |> Store.makeElmishSimple init update ignore

    Browser.Dom.document.addEventListener ("keydown", (fun event -> event :?> KeyboardEvent |> KeyDown |> dispatch))

    Html.div [ disposeOnUnmount [ model ]

               onKeyDown (fun event -> KeyDown event |> dispatch) []

               Bind.el (
                   (model |> Store.map getGameState),
                   (fun gs ->
                       match gs with
                       | Start -> startView dispatch
                       | Playing -> playView model dispatch
                       | _ -> noopView ())
               )

               Bind.el (
                   (model |> Store.map getPlayMusic |> Store.distinct),
                   (fun s ->
                       match s with
                       | false -> Html.text ""
                       | true ->
                           Html.audio [ Attr.autoPlay true
                                        Attr.loop true
                                        Attr.src "sound/level1-SilentDarkNight.mp3" ])
               )

               Bind.each ((model |> Store.map getPlayingSounds), createAudioTag dispatch, getSoundId) ]

// Start the app
view () |> Program.mountElement "sutil-app"

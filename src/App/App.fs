module App

open System
open Sutil
open Sutil.DOM
open Sutil.Attr
open Browser.Types

// --- DOMAIN TYPES ---
type GameState =
    | Start
    | Playing
    | GameLost
    | GameWon

type Direction =
    | Left
    | Up
    | Right
    | Down
    | Noop

type SoundState = 
    | Queued
    | Started

type TileType =
    | Wall
    | Floor

type PositionData = {
    X:int
    Y:int
}

type DrawableData = {
    Icon: string
}

type Component =
    | Position of PositionData
    | Drawable of DrawableData

type Entity = {
    Id:Guid
    Components: Component list
}

type DrawableEntity = {
    DrawableData : DrawableData
    Position : PositionData
}

type World = TileType[]
type Sound = {
    SoundId : Guid
    FileName : string
    SoundState : SoundState
}
let getSoundId (sound : Sound) = sound.SoundId

let worldWidth = 8
let worldHeight = 8
let gridSize = 64

let createWorld () =
    let allWalls = seq { 1 .. worldWidth } |> Seq.map (fun _ -> Wall) |> Seq.toList
    let withFloors = [Wall; Floor; Floor; Floor; Floor; Floor; Floor; Wall;]
    allWalls @ withFloors @ withFloors @ withFloors @ withFloors @ withFloors @ withFloors @ allWalls |> List.toArray

let coordinatesToArrayIndex x y = 
    x + (y * worldWidth)

let spawnPlayer () : Entity=
    {
        Id = Guid.NewGuid()
        Components = [Drawable {Icon = "🧝"}; Position{X=1;Y=1;} ]
    }

let spawnOgre x y : Entity=
    {
        Id = Guid.NewGuid()
        Components = [Drawable {Icon = "👹"}; Position{X=x;Y=y;} ]
    }

type Model =
    { GameState: GameState
      PlayerDirection: Direction
      PlayingSound: string option
      PlayingSounds: Sound list
      PlayMusic: bool
      World : World
      Entities : Entity list
      EntitiesToDraw : DrawableEntity list }

let init () : Model =
    { GameState = Start
      PlayerDirection = Noop
      PlayingSound = None
      PlayingSounds = []
      PlayMusic = false
      World = createWorld()
      Entities = []
      EntitiesToDraw = [] }

let keyToDirection (event: KeyboardEvent) =
    match event.code with
    | "ArrowLeft" -> Left
    | "ArrowUp" -> Up
    | "ArrowRight" -> Right
    | "ArrowDown" -> Down
    | _ -> Noop

// --- MODEL VALUE HELPERS ---
let getGameState m = m.GameState
let getDirection m = m.PlayerDirection
let getPlayingSound m = m.PlayingSound
let getPlayingSounds m = m.PlayingSounds
let getPlayMusic m = m.PlayMusic
let getWorld m = m.World
let getEntitiesToDraw m = m.EntitiesToDraw

// --- MESSAGES ---
type Message =
    | StartGame
    | KeyDown of KeyboardEvent
    | ToggleMusic
    | PlaySound of string // TODO Make sounds DU?
    | SoundPlayed of Guid
    | SoundPlaying of Guid

let isPosition (c:Component) =
    match c with 
    | Position _ -> true
    | _ -> false

let isDrawable (c:Component) =
    match c with 
    | Drawable _ -> true
    | _ -> false

let getPosition (c:Component) : PositionData =
    match c with
    | Position x -> x
    | _ -> failwith "Is not Position"

let getDrawable (c:Component) : DrawableData =
    match c with
    | Drawable x -> x
    | _ -> failwith "Is not Drawable"

let drawableSystem (entities : Entity list) : DrawableEntity list =
    List.filter (fun (e:Entity) -> List.exists isPosition e.Components && List.exists isDrawable e.Components) entities
    |> List.map (fun e -> 
        let position = List.find isPosition e.Components |> getPosition
        let drawable = List.find isDrawable e.Components |> getDrawable
        
        {
            DrawableData = drawable
            Position = position
        }
        ) 

let tick (model : Model) =
    let entitiesToDraw = drawableSystem model.Entities
    {model with EntitiesToDraw = entitiesToDraw}

// --- MESSAGE HANDLING, MODEL UPDATES ---
let update (msg: Message) (model: Model) : Model =
    match msg with
    | StartGame -> 
        let monsterX = Random.randomInt 2 (worldWidth-1)
        let monsterY = Random.randomInt 2 (worldHeight-1)
        { model with 
            GameState = Playing; 
            Entities = [
                spawnPlayer()
                spawnOgre monsterX monsterY
            ] }
    | KeyDown event ->
        if model.GameState = Playing then
            let direction = event |> keyToDirection
            tick { model with PlayerDirection = direction }
        else
            model
        
    | ToggleMusic -> { model with PlayMusic = not model.PlayMusic }
    | PlaySound soundFileName -> 
        let newSound = {
            SoundId = Guid.NewGuid()
            FileName = soundFileName
            SoundState = Queued
        }
        { model with PlayingSounds = newSound :: model.PlayingSounds }
    | SoundPlayed soundId -> {model with PlayingSounds = List.filter (fun s -> s.SoundId <> soundId ) model.PlayingSounds}
    | SoundPlaying soundId -> 
        let setPlayingSoundToStarted s = 
            if (s.SoundId = soundId) then
                {s with SoundState = Started}
            else 
                s
        { model with PlayingSounds = List.map setPlayingSoundToStarted model.PlayingSounds}

// --- VIEWS ---
let startView (dispatch) =
    Html.div [ Html.h1 "Saving Santa's"
               Html.div [ Html.p "Help Santa to reclaim his basement and collect all the presents!"
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

let drawWorld (world : World) =
    let floor () = Html.span [Attr.className "floor"; Html.text " "]
    let wall () = Html.span [Attr.className "wall"; Html.text "🧱"]
    let rows = Array.chunkBySize worldWidth world
    let createCell cell =
        match cell with
        | Floor -> floor ()
        | Wall -> wall ()
    let createRow row =
        Html.div (Array.map createCell row)

    fragment (Array.map createRow rows)

let drawEntity (drawableEntity : DrawableEntity) : SutilElement =
    Html.div [  Html.text drawableEntity.DrawableData.Icon 
                style [Css.positionAbsolute; Css.left (gridSize * drawableEntity.Position.X); Css.top (gridSize * drawableEntity.Position.Y); ]]

let playView (model: IStore<Model>) (dispatch: Dispatch<Message>) =
    Html.div [ Html.div "PLAY"
               Bind.el ((model |> Store.map getWorld), drawWorld)
               Bind.each ((model |> Store.map getEntitiesToDraw), drawEntity)
               ]

let noopView () = Html.h1 "NOTHING HERE YET"

let createAudioTag dispatch (sound: IObservable<Sound>) =
    Bind.el (sound, fun s -> 
                let shouldPlay = (s.SoundState = Queued)  
                Html.audio [  
                    on "play" (fun _ -> s.SoundId |> SoundPlaying |> dispatch) []  
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

               Bind.el
                   ((model |> Store.map getPlayMusic |> Store.distinct),
                    (fun s ->
                        match s with
                        | false -> Html.text ""
                        | true ->
                            Html.audio [ Attr.autoPlay true
                                         Attr.loop true
                                         Attr.src "sound/level1-SilentDarkNight.mp3" ]))

               Bind.each ((model |> Store.map getPlayingSounds), createAudioTag dispatch, getSoundId)
                ]

// Start the app
view () |> Program.mountElement "sutil-app"

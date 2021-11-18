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

type Sound = {
    SoundId : Guid
    FileName : string
    SoundState : SoundState
}
let getSoundId (sound : Sound) = sound.SoundId

type Model =
    { GameState: GameState
      PlayerDirection: Direction
      PlayingSound: string option
      PlayingSounds: Sound list
      PlayMusic: bool }

let init () : Model =
    { GameState = Start
      PlayerDirection = Noop
      PlayingSound = None
      PlayingSounds = []
      PlayMusic = false }

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

// --- MESSAGES ---
type Message =
    | StartGame
    | KeyDown of KeyboardEvent
    | ToggleMusic
    | PlaySound of string // TODO Make sounds DU?
    | SoundPlayed of Guid
    | SoundPlaying of Guid

// --- MESSAGE HANDLING, MODEL UPDATES ---
let update (msg: Message) (model: Model) : Model =
    match msg with
    | StartGame -> { model with GameState = Playing }
    | KeyDown event ->
        let direction = event |> keyToDirection
        { model with PlayerDirection = direction }
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

let playView (model: IStore<Model>) (dispatch: Dispatch<Message>) =
    Html.div [ Html.div "PLAY"
               Bind.el (
                   (model |> Store.map getDirection),
                   (fun d ->
                       Html.div [ Html.button [ type' "button"
                                                text "Toggle music"
                                                onClick (fun _ -> ToggleMusic |> dispatch) [] ]
                                  Html.button [ type' "button"
                                                text "Make a sound"
                                                onClick (fun _ -> PlaySound "pickup-Enter.wav" |> dispatch) [] ]
                                  Html.button [ type' "button"
                                                text "Make another sound"
                                                onClick (fun _ -> PlaySound "gameover-Event3.mp3" |> dispatch) [] ] ])
               ) ]

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

               // See Sutil.Styling for more advanced styling options
               style [ Css.fontFamily "Arial, Helvetica, sans-serif"
                       Css.margin 20 ]

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

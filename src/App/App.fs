module App

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
    | None


type Model =
    { GameState: GameState
      PlayerDirection: Direction }

let init () : Model =
    { GameState = Start
      PlayerDirection = None }

let keyToDirection (event: KeyboardEvent) =
    match event.code with
    | "ArrowLeft" -> Left
    | "ArrowUp" -> Up
    | "ArrowRight" -> Right
    | "ArrowDown" -> Down
    | _ -> None

// --- MODEL VALUE HELPERS ---
let getGameState m = m.GameState
let getDirection m = m.PlayerDirection

// --- MESSAGES ---
type Message =
    | StartGame
    | KeyDown of KeyboardEvent

// --- MESSAGE HANDLING, MODEL UPDATES ---


let update (msg: Message) (model: Model) : Model =
    match msg with
    | StartGame -> { model with GameState = Playing }
    | KeyDown event ->
        let direction = event |> keyToDirection
        { model with PlayerDirection = direction }

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
                          Html.button [ class' "button"
                                        text "Save Santa!"
                                        onClick (fun _ -> dispatch StartGame) [] ] ] ]

let playView (model: IStore<Model>) (dispatch: Dispatch<Message>) =
    Html.div [ Html.div "PLAY"
               Bind.el ((model |> Store.map getDirection), (fun d -> Html.div "meh")) ]

let noopView () = Html.h1 "NOTHING HERE YET"

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
                       | Start -> startView (dispatch)
                       | Playing -> playView model dispatch
                       | _ -> noopView ())
               ) ]

// Start the app
view () |> Program.mountElement "sutil-app"

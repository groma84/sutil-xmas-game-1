module Model 

open Types
open Components

type Model = { 
    GameState: GameState
    PlayerDirection: Direction
    PlayingSound: string option
    PlayingSounds: Sound list
    PlayMusic: bool
    World : World
    Entities : Entity list
    EntitiesToDraw : DrawableEntity list }

// --- MODEL VALUE HELPERS ---
let getGameState m = m.GameState
let getDirection m = m.PlayerDirection
let getPlayingSound m = m.PlayingSound
let getPlayingSounds m = m.PlayingSounds
let getPlayMusic m = m.PlayMusic
let getWorld m = m.World
let getEntitiesToDraw m = m.EntitiesToDraw
module Types

open System

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

type World = TileType[]
type Sound = {
    SoundId : Guid
    FileName : string
    SoundState : SoundState
}
let getSoundId (sound : Sound) = sound.SoundId
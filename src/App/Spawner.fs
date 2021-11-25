module Spawner

open System
open Components

let newId () = Guid.NewGuid()

let spawnPlayer () : Entity =
    { Id = newId()
      Components =
        [ Player { Inventory = [] }
          Drawable { Icon = "🧝" }
          Position { X = 1; Y = 1 }
          MoveByKeyboard
          BlocksMovement ] }

let spawnOgre x y : Entity =
    { Id = newId()
      Components =
        [ Enemy
          Drawable { Icon = "👹" }
          Position { X = x; Y = y }
          MoveRandomly
          BlocksMovement ] }

let spawnQuestItem x y (icon : string) : Entity =
    { Id = newId()
      Components =
        [ QuestItem
          Drawable { Icon = icon }
          Position { X = x; Y = y }
          CanBePickedUp ] }

let spawnLevelExit x y : Entity =
    { Id = newId()
      Components =
        [ LevelExit
          Drawable { Icon = "⏫" }
          Position { X = x; Y = y }
        ]
    }

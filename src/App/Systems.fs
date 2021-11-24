module Systems

open System
open Sutil
open Constants
open Types
open Components
open Query

let drawableSystem (entities: Entity list) : DrawableEntity list =
    entities
    |> hasComponents [isPosition; isDrawable;] 
    |> List.map (fun entity ->
        let (pos, draw) = getComponents2 (position, drawable) entity

        { DrawableData = draw
          Position = pos })

let moveInDirection (world : World) (blockingEntities : Entity list) (movingEntity : Entity) (direction : Direction) : Entity =
    let currentPosition = getComponent position movingEntity
    let sanitizePosition pos = 
        {pos with X = Math.Min(worldWidth, Math.Max(0, pos.X)); Y = Math.Min(worldHeight, Math.Max(0, pos.Y))}

    let tryPosition =
        match direction with
        | Up -> {currentPosition with Y = currentPosition.Y - 1} 
        | Down -> {currentPosition with Y = currentPosition.Y + 1} 
        | Left -> {currentPosition with X = currentPosition.X - 1} 
        | Right -> {currentPosition with X = currentPosition.X + 1} 
        | Noop -> currentPosition
        |> sanitizePosition 

    let isWall {X = x; Y = y} =
        let worldIndex = coordinatesToArrayIndex x y
        world.[worldIndex] = Wall
    
    let isBlockedByOtherEntity {X = x; Y = y} =
        let blockingPositions = List.map (getComponent position) blockingEntities
        List.exists (fun {X = x'; Y = y'} -> x = x' && y = y') blockingPositions

    let newPosition = 
        if (not (isWall tryPosition || isBlockedByOtherEntity tryPosition)) then
            tryPosition
        else
            currentPosition
        |> Position

    replaceComponent isPosition movingEntity newPosition

let moveRandomlySystem world blockingEntities movingEntities =
    let randomDirection () =
        match Random.randomInt 0 3 with
        | 0 -> Down
        | 1 -> Left
        | 2 -> Right
        | 3 -> Up
        | _ -> Noop
    
    let (_, movedEntities) = List.fold 
                                (fun (updatedBlockers, alreadyMoved) movingEntity -> 
                                    let movedEntity = moveInDirection world updatedBlockers movingEntity (randomDirection ())
                                    let updatedMoved =  movedEntity :: alreadyMoved
                                    let blockersAfter = replaceEntity updatedBlockers movedEntity
                                    (blockersAfter, updatedMoved)
                                )
                                (blockingEntities, [])
                                movingEntities
    
    movedEntities


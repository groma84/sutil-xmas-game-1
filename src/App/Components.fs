module Components

open System



type PositionData = {
    X : int
    Y : int
}

type DrawableData = {
    Icon : string
}

type DrawableEntity = {
    DrawableData : DrawableData
    Position : PositionData
}


type Component =
    | Player of PlayerData
    | Enemy
    | Position of PositionData
    | Drawable of DrawableData
    | MoveByKeyboard
    | MoveRandomly
    | BlocksMovement
    | QuestItem
    | CanBePickedUp
and PlayerData = {
    Inventory : Component list
}

let isPlayer (c: Component) =
    match c with
    | Player _ -> true
    | _ -> false   
let getPlayer (c: Component) : PlayerData =
    match c with
    | Player x -> x
    | _ -> failwith "Is not Player"

let isEnemy (c: Component) =
    match c with
    | Enemy -> true
    | _ -> false  

let isPosition (c: Component) =
    match c with
    | Position _ -> true
    | _ -> false
let getPosition (c: Component) : PositionData =
    match c with
    | Position x -> x
    | _ -> failwith "Is not Position"
let position = (isPosition, getPosition)

let isDrawable (c: Component) =
    match c with
    | Drawable _ -> true
    | _ -> false
let getDrawable (c: Component) : DrawableData =
    match c with
    | Drawable x -> x
    | _ -> failwith "Is not Drawable"
let drawable = (isDrawable, getDrawable)

let isMoveByKeyboard (c: Component) =
    match c with
    | MoveByKeyboard -> true
    | _ -> false 

let isMoveRandomly (c: Component) =
    match c with
    | MoveRandomly -> true
    | _ -> false 

let isBlocksMovement (c: Component) =
    match c with
    | BlocksMovement -> true
    | _ -> false 

let isQuestItem (c: Component) =
    match c with
    | QuestItem -> true
    | _ -> false 

let isCanBePickedUp (c: Component) =
    match c with
    | CanBePickedUp -> true
    | _ -> false 

[<NoEquality>]
[<NoComparison>]
type Entity = {
    Id : Guid
    Components : Component list
}
let entityEq e1 e2 =
    e1.Id = e2.Id
let replaceEntity (allEntities : Entity list) (changedEntity : Entity) : Entity list =
    changedEntity :: List.filter (fun e -> not <| entityEq e changedEntity) allEntities
    
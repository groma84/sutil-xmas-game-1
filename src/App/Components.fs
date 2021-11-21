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
    | Position of PositionData
    | Drawable of DrawableData

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

type Entity = {
    Id : Guid
    Components : Component list
}

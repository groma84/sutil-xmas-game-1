module Components

open System

type PositionData = {
    X : int
    Y : int
}

type DrawableData = {
    Icon : string
}

type Component =
    | Position of PositionData
    | Drawable of DrawableData

type Entity = {
    Id : Guid
    Components : Component list
}

type DrawableEntity = {
    DrawableData : DrawableData
    Position : PositionData
}
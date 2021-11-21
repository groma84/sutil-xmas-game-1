module Query

open Components

type Filter = Component -> bool
type Get<'a> = Component -> 'a

type Query<'a> = (Filter * Get<'a>)

let replaceComponent (findComponent : Filter) (entity : Entity) (newComponent : Component) : Entity = 
    {entity with Components = newComponent :: List.filter (fun c -> not <| findComponent c) entity.Components}

let hasComponent (componentFilter: Filter) (entities: Entity list) : Entity list = 
    let hasComponent entity = List.exists componentFilter entity.Components
    entities |> List.filter hasComponent 

let hasComponents (componentFilters: Filter list) (entities: Entity list) : Entity list = 
    let hasComponent entity filterFn = List.exists filterFn entity.Components
    let hasAll entity = 
        List.map (hasComponent entity) componentFilters
        |> List.reduce (fun x y -> x && y)

    entities |> List.filter hasAll


let getComponent<'a> ((componentFilter, componentDataGet) : Query<'a>) (entity : Entity) : 'a =
    let comp = List.filter componentFilter entity.Components
    match comp with
    | [c] -> componentDataGet c
    | _ -> failwith "Component not found or component exists multiple times"

let getComponents2<'a, 'b> 
    (
        ((componentFilter1, componentDataGet1) : Query<'a>), 
        ((componentFilter2, componentDataGet2) : Query<'b>) 
    )
    (entity : Entity) 
    : ('a * 'b) =
    (getComponent (componentFilter1, componentDataGet1) entity, getComponent (componentFilter2, componentDataGet2) entity)
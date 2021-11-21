module Query

open Components

let hasComponent (componentFilter: Component -> bool) (entities: Entity list) : Entity list = 
    let hasComponent entity = List.exists componentFilter entity.Components
    entities |> List.filter hasComponent 

let hasComponents (componentFilters: (Component -> bool) list) (entities: Entity list) : Entity list = 
    let hasComponent entity filterFn = List.exists filterFn entity.Components
    let hasAll entity = 
        List.map (hasComponent entity) componentFilters
        |> List.reduce (fun x y -> x && y)

    entities |> List.filter hasAll


let getComponent<'a> (componentFilter: Component -> bool) (componentDataGet: Component -> 'a) (entity : Entity) : 'a =
    let comp = List.filter componentFilter entity.Components
    match comp with
    | [c] -> componentDataGet c
    | _ -> failwith "Component not found or component exists multiple times"

let getComponents2<'a, 'b> 
    (
        ((componentFilter1, componentDataGet1) : ((Component -> bool) * (Component -> 'a))), 
        ((componentFilter2, componentDataGet2) : ((Component -> bool) * (Component -> 'b))) 
    )
    (entity : Entity) 
    : ('a * 'b) =
    (getComponent componentFilter1 componentDataGet1 entity, getComponent componentFilter2 componentDataGet2 entity)
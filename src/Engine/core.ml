open Math.Transform
open Math.Vector2
type rectangle = {height : int ; width : int}


let dt_ref = ref 0.0;;
let dt () = !dt_ref;;
class entity  = 
  object(self)
    val mutable components : component list = []
    val mutable transform : transform = {position = 0.0,0.0; scale = 1.0,1.0; angle = 0.0; depth = 0.0}
    val mutable parent : entity option = None
    method set_transform t = transform <- t
    method get_world_transform : transform= 
    match parent with 
      | Some p -> let p_transform = p#get_world_transform in {transform with position = Math.Vector2.(+) transform.position p_transform.position; angle = transform.angle +. p_transform.angle;
      scale = mul transform.scale  p_transform.scale }
      | None -> transform
    method set_parent p = parent <- Some p
    method add_component c = 
      components <- c::components
    method get_components = components
  end

and virtual component (entity :  entity )= 
  object(self)
    val mutable _entity : entity = entity 
    method get_entity = _entity
    method init () = ()
    method update () = ()
  end

type action = unit -> unit

class virtual actor (cd : int) (acts : (string * action) list)= 
  object(self)
    inherit entity
    val mutable cooldown = cd
    val mutable actions =acts
    val mutable is_ready  = false
    method virtual take_action : unit -> unit
  end

class collision_box (box : rectangle)= 
  object(self)
    val mutable box : rectangle = box
  end

let iter_on_component_update = List.iter (fun c ->c#update ()) 
let iter_on_component_init = List.iter (fun c ->c#init ()) 
class scene (entities : entity list)  = 
  object(self)
    val mutable entities  = entities
    (** TODO : optimiser gameUpdate *)
    method sceneUpdate ()= 
    List.iter ( fun e -> iter_on_component_update (e#get_components ) ) entities 
    initializer (List.iter ( fun e -> iter_on_component_init (e#get_components)) entities
)
  end

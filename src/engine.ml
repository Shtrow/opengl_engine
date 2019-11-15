type rectangle = {height : int ; width : int}
type position = {x : int ; y : int}
type scale = {factor_height : int ; factor_width : int}

class entity  (components : component list)= 
  object(self)
    val mutable components : component list = components
    method add_component c = 
      components <- c::components
    method get_components = ref components
    method get_component tag = ref (List.find (fun c -> String.equal c#get_tag tag) components)
  end

and virtual component (entity :  entity ref) (tag : string)= 
  object(self)
    val mutable _entity : entity ref = entity 
    val mutable tag : string = tag 
    method get_entity_ref = _entity
    method get_tag = tag
    method init : unit = ()
    method update : unit = ()
  end

class virtual action (tag : string) = 
  object(self)
    val mutable tag : string = tag 
    method get_tag : string = tag
    method virtual perform : unit 
  end

class virtual actor (components : component list) (cd : int) (acts : action list)= 
  object(self)
    inherit entity components
    val mutable cooldown = cd
    val mutable actions =acts
    val mutable is_ready  = false
    method get_action (tag : string) = ref (List.find (fun c -> String.equal c#get_tag tag) actions)
    method virtual take_action : unit  
  end

class collision_box (box : rectangle)= 
  object(self)
    val mutable box : rectangle = box
  end

class transform (entity : entity ref)= 
  object(self)
    inherit component entity "transform"
    method update = ()
  end

let iter_on_component_update = List.iter (fun c ->c#update) 
let iter_on_component_init = List.iter (fun c ->c#init) 
class scene (entities : entity list)  = 
  object(self)
    val mutable entities  = entities
    (** TODO : optimiser gameUpdate *)
    method sceneUpdate = 
    List.iter ( fun e -> iter_on_component_update !(e#get_components)) entities
    initializer (List.iter ( fun e -> iter_on_component_init !(e#get_components)) entities
)
  end

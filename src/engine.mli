type rectangle = {height : int ; width : int}
type position = {x : int ; y : int}
type scale = {factor_height : int ; factor_width : int}

class entity  : component list ->
  object
    val mutable components : component list
    method add_component : component -> unit
    method get_components : (component list) ref
    method get_component : string -> component ref
  end
and virtual component  : entity ref -> string ->
  object
    val mutable _entity : entity ref
    val mutable tag : string
    method get_entity_ref : entity ref
    method get_tag : string
    method init : unit 
    method update : unit 
  end

class virtual action : string ->
  object 
    val tag : string
    method get_tag : string 
    method virtual perform : unit
  end 

class virtual actor : component list -> int -> action list ->
  object
    inherit entity
    val mutable cooldown : int
    val mutable actions : action list
    val mutable is_ready : bool
    method get_action : string -> action ref
    method virtual take_action : unit
  end 

class collision_box :  rectangle ->
  object 
    val box : rectangle
  end

class transform  : entity ref ->
  object 
    inherit component
    method update : unit
  end

class scene : entity list ->
  object
    val mutable entities : entity list
    method sceneUpdate : unit
  end

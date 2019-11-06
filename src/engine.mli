type rectangle = {height : int ; width : int}
type position = {x : int ; y : int}
type scale = {factor_height : int ; factor_width : int}

class entity  : component list ->
  object
    val mutable components : component list
    method add_component : component -> unit
    method get_components : (component list) ref
  end
and virtual component  : entity ref ->
  object
    val mutable _entity : entity ref
    method get_entity_ref : entity ref
    method update : unit 
  end

class virtual action : 
  object 
    method virtual perform : unit
  end 

class virtual actor : component list -> int -> action list ->
  object
    inherit entity
    val mutable cooldown : int
    val mutable actions : action list
    val mutable is_ready : bool
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
    method gameUpdate : unit
  end

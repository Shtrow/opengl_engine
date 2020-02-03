type rectangle = {height : int ; width : int}

val dt : unit -> float
(* temporary *)
val dt_ref: float ref

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
    method init : unit 
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
    method sceneUpdate : unit
  end

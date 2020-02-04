type rectangle = {height : int ; width : int}

val dt : unit -> float
(* temporary *)
val dt_ref: float ref

class entity  :
  object
    val mutable components : component list
    method set_parent : entity -> unit
    method add_component : component -> unit
    method get_components : (component list)
    method get_world_transform : Math.Transform.transform
    method set_transform : Math.Transform.transform -> unit
  end
and virtual component  : entity ->
  object
    val mutable _entity : entity
    method get_entity : entity
    method init : unit -> unit
    method update : unit ->unit
  end

type action  = unit -> unit

class virtual actor : int -> (string *action) list ->
  object
    inherit entity
    val mutable cooldown : int
    val mutable actions : (string *action)  list
    val mutable is_ready : bool
    method virtual take_action : unit -> unit
  end 

class collision_box :  rectangle ->
  object 
    val box : rectangle
  end

(* class scene : entity list ->
  object
    val mutable entities : entity list
    method sceneUpdate : unit
  end *)

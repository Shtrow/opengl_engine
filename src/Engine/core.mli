type rectangle = {height : int ; width : int}

val dt : unit -> float

(* Only needed in game loop *)
val update_dt : float -> unit

class entity  : ?parent:entity ->
         unit ->
  object
    val mutable components : component list
    (* Entity transform is relative to his parent *)
    method set_parent : entity -> unit
    method add_component : component -> unit
    method get_components : (component list)
    (* Return world relative transform *)
    method get_world_transform : Math.Transform.transform
    method get_transform : Math.Transform.transform
    method set_transform : Math.Transform.transform -> unit
  end
and virtual component  : entity ->
  object
    val mutable _entity : entity
    method get_entity : entity
    (* Called onece a the beginning *)
    method init : unit -> unit
    (* Called each frame *)
    method update : unit ->unit
  end

type action  = unit -> unit

type direction = North | South | East | West

val dir_to_vec : direction -> Math.Vector2.vector
val dir_to_angle : direction -> float

class virtual actor : ?parent:entity -> int -> (string* (actor -> unit)) list ->
  object
    inherit entity
    val mutable cooldown : int
    val mutable is_ready : bool
    (* The cell where the actor is *)
    val mutable position : (int*int)
    val my_actions : (string* (actor -> unit)) list
    (* take_action is called every turn for every actor *)
    method virtual take_action : unit -> unit
    method get_action : string -> (actor -> unit)
    method set_position : (int*int) -> unit
    method get_position : unit -> (int*int)
    method get_direction : unit -> direction
    method set_direction : direction -> unit
  end

(* Instance of this component directly draw the animation at the transform associated.
This is not enough for multiple texture object (like terrain). Use Render.animRenderer instead
  *)
class virtual renderComponent : entity -> (unit -> Render.animRenderer) ->
object 
  val mutable anim : Render.animRenderer option
  method get_entity : entity
  method init : unit -> unit
  method update : unit -> unit
end

class collision_box :  rectangle ->
  object 
    val box : rectangle
  end

class scene : entity list ->
  object
    val mutable entities : entity list
    method sceneUpdate : unit -> unit
  end




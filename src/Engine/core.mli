type rectangle = {height : int ; width : int}

val dt : unit -> float

(* Only needed in game loop *)
val update_dt : float -> unit

class entity  : ?parent:entity -> string ->
         unit ->
  object
    val mutable components : component list
    (* Entity transform is relative to his parent *)
    method set_parent : entity -> unit
    method add_component : component -> unit
    method get_components : (component list)
    (* Return world relative transform *)
    method is_activated : bool
    method get_tag : string
    method deactivate : unit -> unit
    method activate : unit -> unit
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
val vec_to_dir : Math.Vector2.vector -> direction
val dir_to_angle : direction -> float
val back : direction -> direction

type player_state = IdleKnife | KnifeAttack | IdleGun | Aiming
class virtual actor : ?parent:entity -> string -> int -> (string* (actor -> unit)) list ->
  object
    inherit entity
    val mutable cooldown : int
    val mutable state : player_state
    val mutable is_ready : bool
    (* The cell where the actor is *)
    val mutable current_cd :int 
    val mutable position : (int*int)
    val my_actions : (string* (actor -> unit)) list
    (* take_action is called every turn for every actor *)
    method is_dead : bool
    method state : player_state
    method set_state : player_state -> unit
    method kill : bool -> unit
    method is_ready: unit -> bool
    method reset_cd: unit-> unit
    method virtual take_action : unit -> unit
    method decrement_cd :unit -> unit
    method get_action : string -> (actor -> unit)
    method set_position : (int*int) -> unit
    method get_position : unit -> (int*int)
    method get_direction : unit -> direction
    method set_direction : direction -> unit
  end

(* Instance of this component directly draw the animation at the transform associated.
This is not enough for multiple texture object (like terrain). Use Render.animRenderer instead
  *)
class virtual renderComponent : entity -> Render.animRenderer Lazy.t ->
object 
  val mutable anim : Render.animRenderer option
  method get_entity : entity
  method init : unit -> unit
  method update : unit -> unit
  method set_sprite_coord : Render.spriteCoord -> unit
  method get_render_anim : unit -> Render.animRenderer
end

class collision_box :  rectangle ->
  object 
    val box : rectangle
  end

class scene : entity list -> actor list ->
  object
    val mutable entities : entity list
    method sceneUpdate : unit -> unit
    method get_actors : actor list 
    method next_turn : unit -> unit
  end

val scene_ref : scene option ref



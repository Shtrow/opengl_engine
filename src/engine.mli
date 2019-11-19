type rectangle = {height : int ; width : int}
type vector2 = {x : int ; y : int}
type scale = {factor_height : int ; factor_width : int}
type direction = Left|Right|Up|Down;;
type cell = int*int

(* Si un component a besoin d'implémenté un accesseur  *)
type component_extension_type = ..
type component_extension_type += Dummy
val dt : unit -> float

class entity  : component list ->
  object
    val mutable components : component list
    method add_component : component -> unit
    method get_components : component list
    method get_component : string -> component
  end
and virtual component  : ?component_extension:component_extension_type -> entity -> string ->
  object
    val mutable _entity : entity
    val mutable tag : string
    val mutable c_e : component_extension_type
    method get_component_extension :component_extension_type
    method get_entity : entity
    method get_tag : string
    method init : unit 
    method update : unit
  end

class virtual action : string -> actor ->
  object 
    val tag : string
    val actor : actor
    method get_tag : string 
    method virtual perform : unit
    method get_actor : actor
  end 

and virtual actor : component list -> int -> action list ->
  object
    inherit entity
    val mutable cooldown : int
    val mutable actions : action list
    val mutable board_position : cell
    val mutable direction : direction
    method is_ready : bool
    method get_direction : direction
    method set_direction : direction -> unit
    method get_board_position : cell
    method set_board_position : cell -> unit
    method add_action : action -> unit
    method get_action : string -> action
    method virtual take_action : unit
  end 

class collision_box :  rectangle ->
  object 
    val box : rectangle
  end

class transform  : entity ->
  object 
    inherit component
  end

class scene : entity list -> string ->
  object
    val mutable entities : entity list
    method get_name : string
    method add_entity : entity -> unit
    method scene_update : unit
  end
class game  : scene ->
  object
  val mutable current_scene : scene 
  val mutable scene_list : scene array
  method set_current_scene : int -> unit
  method run : unit -> unit
  end
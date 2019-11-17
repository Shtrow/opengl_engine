type rectangle = {height : int ; width : int}
type vector2 = {x : int ; y : int}
type scale = {factor_height : int ; factor_width : int}
type direction = Left|Right|Up|Down;;
type cell = int*int
val dt : unit -> float

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

class virtual action : string -> actor ref ->
  object 
    val tag : string
    val actor : actor ref
    method get_tag : string 
    method virtual perform : unit
    method get_actor : actor ref
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
    val position : vector2
    method update : unit
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
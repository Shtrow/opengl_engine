type rectangle = {height : int ; width : int}
type vector2 = {x : int ; y : int}
type scale = {factor_height : int ; factor_width : int}
type direction = Left|Right|Up|Down;;
type cell = int*int
let dt_ref = ref 0.;;
let dt () = !dt_ref;;
let framerate = 1./.60.;;
let frame_counter = ref 0;

class entity  (components : component list)= 
  object(self)
    val mutable components : component list = components
    method add_component c = 
      components <- c::components
    method get_components = ref components
    (* TODO : changer getComponent pour qu'il revoie le type le plus bas (gérer ça avec du polymorphisme)  *)
    method get_component tag = ref (List.find (fun c -> String.equal c#get_tag tag) components)
  end

and virtual component (entity :  entity ref) (tag : string) = 
  object(self)
    val mutable _entity : entity ref = entity 
    val mutable tag : string = tag
    method get_entity_ref = _entity
    method get_tag = tag
    method init : unit = ()
    method update : unit = ()
  end

class virtual action (tag : string) (actor: actor ref) = 
  object(self)
    val mutable tag : string = tag 
    val actor = actor
    method get_tag : string = tag
    method virtual perform : unit 
    method get_actor = actor
  end

and virtual actor (components : component list) (cd : int) (acts : action list)= 
  object(self)
    inherit entity components
    val mutable cooldown = cd
    val mutable actions =acts
    val mutable board_position : cell = (0,0)
    val mutable direction = Up
    method is_ready  = cooldown <= 0
    method get_direction = direction
    method set_direction d = direction <- d
    method get_board_position = board_position
    method set_board_position p = board_position <- p
    method add_action a = actions <- a::actions
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
    val position = {x =0; y = 0}
  end

let iter_on_component_update = List.iter (fun c ->c#update) 
let iter_on_component_init = List.iter (fun c ->c#init) 
class scene (entities : entity list) (name:string) = 
  object(self)
    val mutable entities  = entities
    val mutable _name = name
    method get_name = _name
    (** TODO : optimiser gameUpdate *)
    method scene_update = 
    List.iter ( fun e -> iter_on_component_update !(e#get_components)) entities
    initializer (List.iter ( fun e -> iter_on_component_init !(e#get_components)) entities
    )
    method add_entity entity = entities <- entity::entities
  end

class game (scene : scene) =
object(self)
  val mutable current_scene = scene 
  val mutable scene_list = [|scene|]
  method set_current_scene (i:int) = current_scene <- (Array.get scene_list i)
  
  method run () = self#run_loop 0. 0.
  method private run_loop  (last_time: float) (dt_cumulator:float) : unit=
    let t = Sys.time() in
    dt_ref := (t -. last_time);
    (* ici on limite le framerate *)
    if dt_cumulator >= framerate then (
      frame_counter := (!frame_counter+1);
      (current_scene#scene_update; self#run_loop t 0.))
    else
      self#run_loop (t) (dt_cumulator +. dt())
end
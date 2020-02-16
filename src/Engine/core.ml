open Math.Transform
open Math
type rectangle = {height : int ; width : int}


let dt () = Render.dt()
let update_dt f = (Render.ref_dt) := f
class entity ?(parent) () = 
  object(self)
    val mutable components : component list = []
    val mutable transform : transform = {position = 0.0,0.0; scale = 1.0,1.0; angle = 0.0; depth = 0.0}
    val mutable parent : entity option = parent
    method set_transform t = transform <- t
    method get_transform = transform
    method get_world_transform : transform= 
    match parent with 
      | Some p -> let p_transform = p#get_world_transform in
       {transform with  angle = transform.angle +. p_transform.angle;
      scale = Vector2.mul transform.scale  p_transform.scale; position =
        Math.Vector2.(+) transform.position p_transform.position
      }
      | None -> transform
    method set_parent p = parent <- Some p
    method add_component c = 
      components <- c::components
    method get_components = components
  end

and virtual component (entity :  entity )= 
  object(self)
    val mutable _entity : entity = entity 
    method get_entity = _entity
    method init () = ()
    method update () = ()
  end

type action = unit -> unit

type direction = North | South | East | West
let vec_to_dir = function
  | 0.,1. -> South
  | 0.,-1. -> North
  | 1.,0. -> East
  | -1.,0. -> West
  |_ -> South

let dir_to_vec = function 
  | North -> Math.Vector2.down
  | South -> Math.Vector2.up
  | East -> Math.Vector2.right
  | West -> Math.Vector2.left
  
let dir_to_angle = function 
  | East -> 270.
  | West -> 90.0
  | North -> 180.
  | South -> 0.
  
let back d = 
  let inv = 
    let v = dir_to_vec d in 
    Vector2.mul_scalar v (-1.0) |> vec_to_dir in inv

class virtual actor ?parent (cd : int) (actions) = 
  object(self)
    inherit entity ?parent:parent ()
    val mutable direction = South
    val mutable cooldown = cd
    val mutable current_cd = cd
    val mutable is_ready  = false
    val mutable position = (0,0)
    val my_actions : (string* (actor -> unit)) list = actions
    method is_ready () = is_ready
    method virtual take_action : unit -> unit
    method decrement_cd () = current_cd <- current_cd -1;
      if current_cd <= 0 then is_ready <- true; 
    method reset_cd () = current_cd <-cooldown
    method get_action name = let d = List.assoc name my_actions in d
    method set_position p = position <- p
    method get_position  () = position
    method get_direction () = direction
    method set_direction d = direction<-d

  end

class collision_box (box : rectangle)= 
  object(self)
    val mutable box : rectangle = box
  end

let iter_on_component_update = List.iter (fun c ->c#update ()) 
let iter_on_component_init = List.iter (fun c ->c#init ()) 
class scene (entities : entity list) actors  = 
  object(self)
    val mutable entities  = entities
    val mutable actors  = actors
    (** TODO : optimiser gameUpdate *)
    method next_turn () = 
      List.iter (fun act ->
        act#decrement_cd ()
      ) actors;
      (* Printf.printf "Next turn" *)

    method sceneUpdate ()= 
      List.iter (fun act -> 
        if act#is_ready () then act#take_action() ; act#reset_cd()
      )actors;
    Render.clear ();
    List.iter ( fun e -> iter_on_component_update (e#get_components ) ) entities 
    initializer (List.iter ( fun e -> iter_on_component_init (e#get_components)) entities ; self#next_turn()
)
  end

class virtual renderComponent entity (animRender) = 
object(self) 
  inherit component entity
  val mutable anim :  Render.animRenderer option = None
  method  init () = 
    anim <- Some (animRender () )
  method update () = 
    (Option.get anim)#draw (Render.to_sprite_coord (self#get_entity#get_world_transform))
end
open Math.Transform
open Math
type rectangle = {height : int ; width : int}


let dt () = Render.dt()
let update_dt f = (Render.ref_dt) := f
class entity ?(parent) tag () = 
  object(self)
    val tag : string = tag
    val mutable activated = true
    val mutable components : component list = []
    val mutable transform : transform = {position = 0.0,0.0; scale = 1.0,1.0; 
        angle = 0.0; depth = 0.0; pivot =0.0,0.0}
    val mutable parent : entity option = parent
    method is_activated = activated
    method get_tag = tag
    method deactivate () = activated <- false
    method activate () = activated <- true
    method set_transform t = transform <- t
    method get_transform = transform
    method get_world_transform : transform= 
    match parent with 
      | Some p -> let p_global_transform = p#get_world_transform in
                  let p_local_transform = p#get_transform in
       {transform with  angle = transform.angle +. p_global_transform.angle;
      scale = Vector2.mul transform.scale  p_global_transform.scale; 
      position =
        let s,c = sin @@ Math.Vector2.degree_to_rad (p_global_transform.angle ), 
          cos @@ Math.Vector2.degree_to_rad (p_global_transform.angle ) in 

        let px,py = Math.Vector2.(+) transform.position p_global_transform.position in 
        let ox,oy = Math.Vector2.(+) p_local_transform.pivot p_global_transform.position in
        let xnew, ynew =
         c *. (px-.ox) -. s *.(py-.oy) +. ox,
         s *. (px-.ox) +. c *.(py-.oy) +. oy in
        let p = Math.Vector2.(+)  (xnew,ynew) (0.0,0.0)
        in p
         (* in  let (x,y) = Math.Vector2.(-) p  p_global_transform.position   in 
         
         let x =  x*. (cos ((Float.pi /. 180.) *. p_global_transform.angle))
         and y = y*. (sin ((Float.pi /. 180.) *. p_global_transform.angle))
         in (x,y) *)
      }
      | None -> 
      transform
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
    Vector2.mul_scalar (-1.0) v  |> vec_to_dir in inv

type player_state = IdleKnife | KnifeAttack | IdleGun | Aiming
class virtual actor ?parent tag (cd : int) (actions) (pos: (int * int)) (dir:direction)= 
  object(self)
    inherit entity ?parent:parent tag ()
    val mutable state = IdleKnife
    method state  = state
    method set_state s = state <- s
    val init_direction = dir
    val mutable direction =  dir
    val init_position = pos
    val mutable position = pos
    val mutable cooldown = cd
    val mutable current_cd = cd
    val mutable is_dead : bool = false
    val mutable is_ready  = false
    val my_actions : (string* (actor -> unit)) list = actions
    method is_dead = is_dead
    method kill b = is_dead <- b
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
    method reset () = 
      direction <- init_direction;
      (self:>entity)#activate();
      is_dead <- false

  end

class collision_box (box : rectangle)= 
  object(self)
    val mutable box : rectangle = box
  end

let iter_on_component_update = List.iter (fun c ->   (c#update ())) 
let iter_on_component_init = List.iter (fun c ->c#init ()) 
class scene (entities : entity list) actors  = 
  object(self)
    val mutable entities  = entities
    val mutable actors  = actors
    method get_actors : actor list = actors
    method reset () = List.iter (fun e -> e#reset()) (actors)
    method next_turn () = 
      List.iter (fun act ->
        act#decrement_cd ()
      ) actors;

    method sceneUpdate ()= 
      List.iter (fun act -> 
        if (not (act#is_dead)) &&act#is_ready () && (act:>entity)#is_activated then
          act#take_action() ; act#reset_cd()
      )actors;
    Render.clear ();
    List.iter ( fun e -> if e#is_activated then  iter_on_component_update (e#get_components ) )
      entities 
    initializer (List.iter ( fun e -> iter_on_component_init (e#get_components)) entities;
      self#next_turn()
)
  end

let scene_ref : scene option ref = ref None 

class virtual renderComponent entity (animRender) = 
object(self) 
  inherit component entity
  val mutable anim :  Render.animRenderer option = None
  val mutable sprite_coord : Render.spriteCoord= {
    position = (0.0,0.0,0.0);
    scale = (0.0,0.0);
    rotation = 0.0
  }
  method  init () = 
    sprite_coord <- (Render.to_sprite_coord (self#get_entity#get_world_transform));

    anim <- Some (Oo.copy (Lazy.force animRender) )
  method update () = 
    (Option.get anim)#draw (Render.to_sprite_coord (self#get_entity#get_world_transform))
  method set_sprite_coord sc = sprite_coord <- sc
  method get_render_anim () = Option.get anim
end

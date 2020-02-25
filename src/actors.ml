open Engine.Core
open Engine.Render
open Engine.Input
open Math

type player_state = Idle | Moving


let scene_ref : scene option ref = ref None ;;

(* Creating animation *)
let player_anims () = let d = new animation [ResourceManager.get_texture "player_idle"] true in
  new animRenderer ["player_idle",d]

let muzzle_anim () = 
  let d = new animation 
  [
    ResourceManager.get_texture "muzzle1";
    ResourceManager.get_texture "muzzle2";
    ResourceManager.get_texture "muzzle3";
    ResourceManager.get_texture "muzzle4";
  ] false in
  d#set_speed 0.01;
  new animRenderer ["fire",d]

let enemy_anims () = let d = new animation [ResourceManager.get_texture "enemy_idle"] true in
     new animRenderer ["enemy",d];;


(* ACTIONS *)
let move actor =
  let v_dir = dir_to_vec @@ actor#get_direction () in 
  let (i,j) as new_p = Math.Vector2.(+) (Math.Vector2.vecF (actor#get_position()))  (v_dir) in 

  Terrain.move_entity (actor) (Math.Vector2.vecI new_p)

let faceNorth actor = 
  actor#set_direction North
let faceSouth actor = 
  actor#set_direction South
let faceEast actor = 
  actor#set_direction East
let faceWest actor = 
  actor#set_direction West

let shoot actor =
  match Terrain.ray_cast (Terrain.front_of (actor#get_position ()) (actor#get_direction()))( dir_to_vec (actor#get_direction())) with 
    | None -> () 
    (* WIP : I deactivate the entity for the moment *)
    | Some ent -> (ent:>entity)#deactivate ()

let backstab actor = 
  let en =  Terrain.front_of (actor#get_position ()) (actor#get_direction()) in 
  match Terrain.get_entity en Terrain.map with
  |None -> () 
  (* Enemy ? *)
  |Some e -> (e:>entity)#deactivate ()

(* Player entity *)
let muzzle = 
object(self)
  inherit entity  ()
end



let muzzleRender = 
object(self)
  inherit renderComponent muzzle muzzle_anim 
end

let player = 
object(self)
  inherit actor ~parent:(Terrain.terrain) 0 [
    ("move",move);
    ("east",faceEast);
    ("west",faceWest);
    ("south",faceSouth);
    ("north",faceNorth);   
    ("shoot",shoot);   
    ("backstab",backstab);   
    ]          
  method take_action () = 
      match Engine.Input.getKeyPressed () with 
        |Some( GLFW.Right) -> (self#get_action "east") (self:>actor)
        |Some( GLFW.Left) -> (self#get_action "west") (self:>actor)
        |Some( GLFW.Up) -> (self#get_action "north") (self:>actor)
        |Some( GLFW.Down) -> (self#get_action "south") (self:>actor)
        |Some (GLFW.Space) ->  (self#get_action "backstab") (self:>actor) ; (self#get_action "move") (self:>actor) ;
        (* is_ready <- false; *)
        (Option.get !scene_ref)#next_turn ()
        |Some (GLFW.W) -> 
          (Option.get !scene_ref)#next_turn ()
        |Some (GLFW.S) ->  (self#get_action "shoot") (self:>actor);
          (* muzzle#set_parent (self:>entity); *)
          let a =  muzzleRender#get_render_anim ()
          in 
          let a = a#get_current_anim in
            a#rewind()
        | _ -> ()
end

let muzzle_behavior = 
object(self)
  inherit component muzzle
  method init () = 
    self#get_entity#set_parent (player:>entity);
    
    let d = self#get_entity#get_transform in 
    let (x,y) = d.position in 
    self#get_entity#set_transform {d with depth = 0.1;
      position = (x+.8.,y+.32.)
    }
end;;

let moveComponent actor init_position= 
object(self)
  inherit component (actor:>entity)
  val mutable l = true

  method move (i, j) = 
   let old_transform = self#get_entity#get_transform in 
    self#get_entity#set_transform @@ {old_transform with position = (float i *. 32., float j *. 32.)}

  method init () = 
    actor#set_position init_position
    
  method update () = 
    let d = actor#get_transform in
    let d = {d with angle = ( dir_to_angle (actor#get_direction()))} in 
    actor#set_transform d ; 
    self#move @@ actor#get_position();

end

let cameraControlComponent = 
object(self)
  inherit component (player:>entity)
  val mutable prev_mouse_pos = (0.0,0.0)
  val mutable hold_mouse_button = false
  method update () = 

    (* Camera behavior *)
    Engine.Render.Camera.zoom (!Engine.Input.scroll_offset);

    if Engine.Input.isMouseButton0Down () then 
      begin
      if not hold_mouse_button then 
        begin 
          let (x, y ) as p  = getMousePosition ()  in  
          prev_mouse_pos <- p ;
          hold_mouse_button <- true ; prev_mouse_pos <-p
        end
      else
      begin
        let new_p = getMousePosition() in 
        let offset = Math.Vector2.(-) new_p prev_mouse_pos in
        Camera.move offset ;
        prev_mouse_pos <- new_p;
      end
      end
    else hold_mouse_button <- false
end

let playerRender = 
object(self)
        (* Note that we use Core.renderCompenent here *)
inherit renderComponent (player:>entity) (player_anims )
end;;






  
(* Enemies *)

let enemy1 = 
object(self)
  inherit actor ~parent:(Terrain.terrain) 1 [
        ("move",move);
    ("east",faceEast);
    ("west",faceWest);
    ("south",faceSouth);
    ("north",faceNorth); 
    ("shoot",shoot); 
  ]
  method is_ready () =
    is_ready
  method take_action () =
    begin
    match Terrain.is_cell_blocked @@ (Vector2.(+) (Vector2.vecF position ) (dir_to_vec (self#get_direction()) ) |> Vector2.vecI) with
    | false -> (self#get_action "move") self ;
    | _ -> self#set_direction (back (self#get_direction()))
    end;
    begin
    let dir = dir_to_vec (self#get_direction()) in 
    (* If an enemy meet the player, he shoot him  *)
      match Terrain.ray_cast  (Terrain.front_of position (self#get_direction())) (dir) with 
        | None -> ()
        |Some e-> (self#get_action "shoot") self
    end;
    is_ready <- false
end;;




let enemyRender = 
object(self)
        (* Note that we use Core.renderCompenent here *)
inherit renderComponent (enemy1:>entity) (enemy_anims )

end;;


(player:>entity)#add_component (playerRender:>component);;
(player:>entity)#add_component (moveComponent player (1,0) :>component);;
(player:>entity)#add_component (cameraControlComponent:>component);;

(enemy1:>entity)#add_component (enemyRender:>component);;
(enemy1:>entity)#add_component (moveComponent enemy1 (4,4) :>component);;


(muzzle:>entity)#add_component (muzzleRender:>component);;
(muzzle:>entity)#add_component (muzzle_behavior:>component);;

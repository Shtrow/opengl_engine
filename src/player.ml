open Engine.Core
open Engine.Render
open Engine.Input

type player_state = Idle | Moving

(* Player actions *)

let move actor =
  let v_dir = dir_to_vec @@ actor#get_direction () in 
  let (i,j) as new_p = Math.Vector2.(+) (Math.Vector2.vecF (actor#get_position()))  (v_dir) in 

  Terrain.move_entity (actor) (Math.Vector2.vecI new_p)

(* If we encounter a wall *)
   

  (* actor#set_position (Math.Vector2.vecI new_p) *)

let faceNorth actor = 
  actor#set_direction North
let faceSouth actor = 
  actor#set_direction South
let faceEast actor = 
  actor#set_direction East
let faceWest actor = 
  actor#set_direction West

(* Player entity *)
let player = 
object(self)
  inherit actor ~parent:(Terrain.terrain) 0 [
    ("move",move);
    ("east",faceEast);
    ("west",faceWest);
    ("south",faceSouth);
    ("north",faceNorth);   
    ]
  method take_action () = 
    is_ready <-true;
    if not is_ready then ()
    else
      match Engine.Input.getKeyPressed () with 
        |Some( GLFW.Right) -> (self#get_action "east") self
        |Some( GLFW.Left) -> (self#get_action "west") self
        |Some( GLFW.Up) -> (self#get_action "north") self
        |Some( GLFW.Down) -> (self#get_action "south") self
        |Some (GLFW.Space) ->  (self#get_action "move") self ;
        | _ -> ()
        (* print_endline "Player moves" *)
    (* This is where you read input and you perform action *)

end



let moveComponent = 
object(self)
  inherit component (player:>entity)
  val mutable l = true

  method move (i, j) = 
   let old_transform = self#get_entity#get_transform in 
    self#get_entity#set_transform @@ {old_transform with position = (float i *. 32., float j *. 32.)}

  method init () = 
    
    player#set_position (3, 3)
    
  method update () = 
    let d = player#get_transform in
    let d = {d with angle = ( dir_to_angle (player#get_direction()))} in 
    player#set_transform d ; 
    self#move @@ player#get_position();

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

(* Creating animation *)
let player_anims () = let d = new animation [ResourceManager.get_texture "player_idle"] true in
     new animRenderer ["player_idle",d];;

let playerRender = 
object(self)
        (* Note that we use Core.renderCompenent here *)
inherit renderComponent (player:>entity) (player_anims )
end;;




(player:>entity)#add_component (playerRender:>component);;
(player:>entity)#add_component (moveComponent:>component);;
(player:>entity)#add_component (cameraControlComponent:>component);;



  

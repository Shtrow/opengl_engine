open Engine.Core
open Engine.Render

(* Player actions *)
let move ()=
  print_endline "Player moves"

(* Player entity *)
let player = 
object(self)
  inherit actor ~parent:(Terrain.terrain) 0
  method take_action () = 
    if not is_ready then ()
    else
    (* This is where you read input and you perform action *)
    ()

end

(* Input exemple *)
let inputPlayer = 
object
inherit component (player:>entity)
method update () =
  let open Math in 
  let speed = 100.0 in 
  if Engine.Input.keyIsDown GLFW.Down then
    begin
    let old = (player:>entity)#get_transform in 
    (player:>entity)#set_transform (Transform.translate old (Vector2.mul_scalar Vector2.up (speed *.dt())))
    end ;
  if Engine.Input.keyIsDown GLFW.Left then
    begin
    let old = (player:>entity)#get_transform in 
    (player:>entity)#set_transform (Transform.translate old (Vector2.mul_scalar Vector2.left (speed *.dt())))
    end;
  if Engine.Input.keyIsDown GLFW.Right then
    begin
    let old = (player:>entity)#get_transform in 
    (player:>entity)#set_transform (Transform.translate old (Vector2.mul_scalar Vector2.right (speed *.dt())))
    end;
  if Engine.Input.keyIsDown GLFW.Up then
    begin
    let old = (player:>entity)#get_transform in 
    (player:>entity)#set_transform (Math.Transform.translate old (Math.Vector2.mul_scalar Vector2.down (speed *.dt())))
    end
    (* ETC... *)
end;;


(* Creating animation *)
let player_anims () = let d = new animation [ResourceManager.get_texture "player_idle"] true in
     new animRenderer ["player_idle",d];;

let playerRender = 
object(self)
        (* Note that we use Core.renderCompenent here *)
inherit renderComponent (player:>entity) (player_anims )
end;;

(player:>entity)#add_component (playerRender:>component);;
(player:>entity)#add_component (inputPlayer);;



  

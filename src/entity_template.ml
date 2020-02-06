open Engine.Core
open Engine.Render

(* DIFINE YOUR ACTIONS HERE *)
let action1 ()=
  print_endline " action1 perform"
let action2 ()=
  print_endline " action2 perform"

(* Player entity *)
let an_entity = 
object(self)
  inherit actor ~parent:(Terrain.terrain) 0
  method take_action () = 
    if not is_ready then ()
    else
    (* This is where you read input (if your player) and you perform action *)
    (* Don't forget to reset cooldown after performing an action *)
    cooldown <- 4;
    ()
end

(* Component *)
let component1 = 
object(self)
  inherit component (an_entity:>entity)
  method init () = 
    (* Init code *)
    ()
  method update () = 
    (* Your behaviour here *)
    ()
end

(* CREATING ANIMATION *)

(* This is a function that return an animRender, we make a function to delay ResourceManager.get_texture call *)
let animRenderer1 () = 
  let anim1 = new animation [
        (* Load any image your want for your animation *)
        ResourceManager.get_texture  "player_idle1"
      ] true

      in
    let anim2 = new animation [
        (* Load any image your want for your animation *)
        ResourceManager.get_texture "wall1";
        ResourceManager.get_texture "wall2";
      ] true in

      (* Setup your animation *)
      anim1#set_speed 20.0 ;  
      anim2#set_speed 12.0 ;
     new animRenderer [("anim1",anim1);("anim2",anim2)]


let render1 = 
object(self)
(* We put the function animRenderer1 () in render so it will call it in init () *)
inherit renderComponent (an_entity:>entity) animRenderer1
end;;

(* Dont forget to add your components to the entity *)
(an_entity:>entity)#add_component component1;;
(an_entity:>entity)#add_component (render1:>component);;




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
    (* Don't forget to decrease cooldown after performing an action *)
    ()
  

end

(* COMPONENT *)
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

(* Creating animation *)
let render1 = 
object(self)
inherit renderComponent (an_entity:>entity)
method init () = 
    let anim1 = new animation [
        (* Load any image your want for your animation *)
        ResourceManager.load_texture_from_file "res/player.png"
      ] true

      in
    let anim2 = new animation [
        (* Load any image your want for your animation *)
        ResourceManager.load_texture_from_file "res/wall/wall1.png";
        ResourceManager.load_texture_from_file "res/wall/wall2.png";
      ] true in

      (* Setup your animation *)
      anim1#set_speed 20.0 ;  
      anim2#set_speed 12.0 ;
  let render_anim = 
     new animRenderer [("anim1",anim1);("anim2",anim2)] in
  self#supply_anim render_anim 
end;;

(* Dont forget to add your components to the entity *)
(an_entity:>entity)#add_component component1;;
(an_entity:>entity)#add_component (render1:>component);;




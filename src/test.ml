open Engine


let debug_entity = 
  object(self)
    inherit entity []
  end
let a_component = 
  object(self)
  inherit component  (ref debug_entity)
  method init = print_string "initialisation"
  method update  = print_string "I'm alive!"
  end;;

debug_entity#add_component a_component;;


let scene = new scene [debug_entity];;

let framerate = 1./.60.;;

let rec gameLoop (scene:scene) (last_time: float) (dt_cumulator:float) : unit= 
  let t = Sys.time() in
  let dt = (t -. last_time) in

  (* ici on limite le framerate *)
  (* if dt_cumulator >= framerate then (scene#sceneUpdate; gameLoop scene t 0.)
  else *)
  scene#sceneUpdate;
  gameLoop scene t (dt_cumulator +. dt)
;;
gameLoop scene 0. 0.;;
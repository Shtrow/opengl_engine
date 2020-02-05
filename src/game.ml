open Engine
open Engine.Core
open Render
open GLFW
open GL
open Player

let window = init_graphic ();;
Input.window_in_for_input window;;

let scene1 = new scene [
  (* Add your entities here *)
  Terrain.terrain; 
  (Player.player:>entity)
  ]

let rec gameLoop  (last_time: float) (dt_cumulator:float) : unit= 
  if  (windowShouldClose (window) )then () else
  let t = Unix.gettimeofday() in
  Engine.Core.update_dt (t -. last_time);
  clear();
  
  scene1#sceneUpdate();
  update_graphic window;

gameLoop t 0.0

;;
gameLoop 0. 0.;;

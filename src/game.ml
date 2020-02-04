open Engine
open Render
open GLFW
open GL
open Player

let window = init_graphic ();;


let processInput window = 
  match getKey~window:window ~key:Escape with
  | true -> setWindowShouldClose ~window:window ~b:true
  |_ -> ();;

let rec gameLoop  (last_time: float) (dt_cumulator:float) : unit= 
  if  (windowShouldClose (window) )then () else
  let t = Unix.gettimeofday() in
  Engine.Core.dt_ref := (t -. last_time);
  processInput window;
     glClearColor ~r:0.5 ~g:0.5 ~b:0.5 ~a:1.0;
    glClear ~mask: [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];
    glEnable (GL_DEPTH_TEST);

  (* ici on limite le framerate *)
  (* Terrain.terrain#get_components *)
  

  update_graphic window;

gameLoop t 0.0

;;
gameLoop 0. 0.;;

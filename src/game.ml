
open Engine
open Engine.Core
open Render
open GLFW
open GL
open Actors

let window = init_graphic ();;
Input.window_in_for_input window;;
Input.init_input_callback window;;

(* Loading textures *)
ResourceManager.load_texture_from_file "player_idle" "res/player.png";;
ResourceManager.load_texture_from_file "enemy_idle" "res/enemy.png";;
ResourceManager.load_texture_from_file "t_grass1" "res/grass1.png";;
ResourceManager.load_texture_from_file "t_grass2" "res/grass2.png";;
ResourceManager.load_texture_from_file "wall1" "res/wall/wall1.png";;
ResourceManager.load_texture_from_file "muzzle1" "res/fx/muzzle1.png";;
ResourceManager.load_texture_from_file "muzzle2" "res/fx/muzzle2.png";;
ResourceManager.load_texture_from_file "muzzle3" "res/fx/muzzle3.png";;
ResourceManager.load_texture_from_file "muzzle4" "res/fx/muzzle4.png";;
print_endline "loui";;
ResourceManager.load_texture_from_file "enemy_dead1" "res/chara/enemy_dead1.png";;
Engine.Render.ResourceManager.load_textures ();;
print_endline "Textures Loaded";;



let scene1 = new scene [
  (* Add your entities here *)
  Terrain.terrain; 
  (Actors.player:>entity);
  (Actors.enemy1:>entity);
  Actors.muzzle_pivot;
  Actors.muzzle;
  ]
 [Actors.enemy1;(Actors.player:>actor);];;

 Actors.scene_ref := Some scene1;;

let rec gameLoop  (last_time: float)  : unit= 
  if  (windowShouldClose (window) )then () else
  let t = Unix.gettimeofday() in
  Engine.Core.update_dt (t -. last_time);
  clear();
  
  scene1#sceneUpdate();

  update_graphic window;

  gameLoop t

;;
let _ = 
(* ResourceManager.load_textures (); *)

gameLoop 0.;

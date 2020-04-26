
open Engine
open Engine.Core
open Render
open GLFW
open GL
open Actors
open Level

let window = init_graphic ();;
Input.window_in_for_input window;;
Input.init_input_callback window;;

(* Loading textures *)
ResourceManager.load_texture_from_file "blood_splash1" "res/fx/blood_splash1.png";;
ResourceManager.load_texture_from_file "blood_splash2" "res/fx/blood_splash2.png";;
ResourceManager.load_texture_from_file "blood_splash3" "res/fx/blood_splash3.png";;
ResourceManager.load_texture_from_file "blood_splash4" "res/fx/blood_splash4.png";;
ResourceManager.load_texture_from_file "blood_splash5" "res/fx/blood_splash5.png";;
ResourceManager.load_texture_from_file "blood_splash6" "res/fx/blood_splash6.png";;
ResourceManager.load_texture_from_file "blood_splash7" "res/fx/blood_splash7.png";;
ResourceManager.load_texture_from_file "bullet" "res/bullet.png";;
ResourceManager.load_texture_from_file "enemy_dead1" "res/chara/enemy_dead1.png";;
ResourceManager.load_texture_from_file "enemy_idle" "res/enemy.png";;
ResourceManager.load_texture_from_file "muzzle1" "res/fx/muzzle1.png";;
ResourceManager.load_texture_from_file "muzzle2" "res/fx/muzzle2.png";;
ResourceManager.load_texture_from_file "muzzle3" "res/fx/muzzle3.png";;
ResourceManager.load_texture_from_file "muzzle4" "res/fx/muzzle4.png";;
ResourceManager.load_texture_from_file "map1" "res/ocaml_game_lvl1.png";;

(*ResourceManager.load_texture_from_file "plant1_1" "res/fx/plant1_1.png";;*)
(*ResourceManager.load_texture_from_file "plant1_2" "res/fx/plant1_2.png";;*)
(*ResourceManager.load_texture_from_file "plant1_3" "res/fx/plant1_3.png";;*)
(*ResourceManager.load_texture_from_file "plant1_4" "res/fx/plant1_4.png";;*)
ResourceManager.load_texture_from_file "player_idle" "res/player.png";;
ResourceManager.load_texture_from_file "player_idle_knife" "res/chara/player_knife.png";;
ResourceManager.load_texture_from_file "player_knife_attack1" "res/chara/player_knife_attack1.png";;
ResourceManager.load_texture_from_file "player_knife_attack2" "res/chara/player_knife_attack2.png";;
ResourceManager.load_texture_from_file "player_knife_attack3" "res/chara/player_knife_attack3.png";;
ResourceManager.load_texture_from_file "player_knife_attack4" "res/chara/player_knife_attack4.png";;
ResourceManager.load_texture_from_file "t_grass1" "res/grass1.png";;
ResourceManager.load_texture_from_file "t_grass2" "res/grass2.png";;
ResourceManager.load_texture_from_file "wall1" "res/wall/wall1.png";;
ResourceManager.load_textures ();;
print_endline "Textures Loaded";;

let current_lvl = Lazy.force lvl1;;

scene_ref := Some (current_lvl);;

let rec gameLoop  (last_time: float)  : unit= 
  if  (windowShouldClose (window) )then () else
  let t = Unix.gettimeofday() in
  Engine.Core.update_dt (t -. last_time);
  clear();
  (current_lvl)#sceneUpdate();

  update_graphic window;

  if Level.scene_completed current_lvl then  
    print_endline "LEVEL CLEAR" else
  gameLoop t

;;
let _ = 
  (* ResourceManager.load_textures (); *)
  Printexc.record_backtrace true;
  gameLoop 0.;

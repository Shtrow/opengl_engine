
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
ResourceManager.load_texture_from_file "player_idle_knife" "res/chara/player_knife.png";;
ResourceManager.load_texture_from_file "player_knife_attack1" "res/chara/player_knife_attack1.png";;
ResourceManager.load_texture_from_file "player_knife_attack2" "res/chara/player_knife_attack2.png";;
ResourceManager.load_texture_from_file "player_knife_attack3" "res/chara/player_knife_attack3.png";;
ResourceManager.load_texture_from_file "player_knife_attack4" "res/chara/player_knife_attack4.png";;
ResourceManager.load_texture_from_file "blood_splash1" "res/fx/blood_splash1.png";;
ResourceManager.load_texture_from_file "blood_splash2" "res/fx/blood_splash2.png";;
ResourceManager.load_texture_from_file "blood_splash3" "res/fx/blood_splash3.png";;
ResourceManager.load_texture_from_file "blood_splash4" "res/fx/blood_splash4.png";;
ResourceManager.load_texture_from_file "blood_splash5" "res/fx/blood_splash5.png";;
ResourceManager.load_texture_from_file "blood_splash6" "res/fx/blood_splash6.png";;
ResourceManager.load_texture_from_file "blood_splash7" "res/fx/blood_splash7.png";;
ResourceManager.load_texture_from_file "enemy_idle" "res/enemy.png";;
ResourceManager.load_texture_from_file "t_grass1" "res/grass1.png";;
ResourceManager.load_texture_from_file "t_grass2" "res/grass2.png";;
ResourceManager.load_texture_from_file "wall1" "res/wall/wall1.png";;
ResourceManager.load_texture_from_file "muzzle1" "res/fx/muzzle1.png";;
ResourceManager.load_texture_from_file "muzzle2" "res/fx/muzzle2.png";;
ResourceManager.load_texture_from_file "muzzle3" "res/fx/muzzle3.png";;
ResourceManager.load_texture_from_file "muzzle4" "res/fx/muzzle4.png";;
ResourceManager.load_texture_from_file "enemy_dead1" "res/chara/enemy_dead1.png";;
ResourceManager.load_texture_from_file "bullet" "res/bullet.png";;
Engine.Render.ResourceManager.load_textures ();;
print_endline "Textures Loaded";;


let enemy1 = add_ennemy (new enemy1 "e1") (4,5) East;;
let enemy2 = add_ennemy (new enemy1 "e2") (5,8) South;;
let enemy3 = add_ennemy (new enemy0 "e3") (2,4) South;;
let scene1 = new scene [
  (* Add your entities here *)
  Terrain.terrain; 
  (enemy3:>entity);
  (enemy1:>entity);
  (enemy2:>entity);
  (Actors.player:>entity);
  Actors.muzzle;
  Actors.bullet;
  Actors.blood_splash;
  ]
 [enemy3;enemy1;enemy2;(Actors.player:>actor);];;

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
Printexc.record_backtrace true;
gameLoop 0.;

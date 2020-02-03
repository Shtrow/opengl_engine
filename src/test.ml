open Engine



type player ={mutable x:int; mutable y:int; mutable dir: string};;


type field = {map:string array array; mutable character: player; a:int; b:int; foes: player array};;


Array.make 10 {x=0; y=0; dir="11"};;

let build x y = {map=Array.make_matrix x y "-"; a=x; b=y; foes= Array.make 10 {x=0; y=0; dir="11"} ; character = {x=0; y=0; dir="4"}};;

let g= build 20 20;;

let dis() = g.map |> Array.iter (fun xs-> xs |> Array.iter (fun x -> print_string x); print_newline ());;

dis();;

let set (x,y) z = g.map |> (fun s -> s.(x).(y) <- z);;

let move (x,y) p = g.map |> (fun s -> set (x+p.x, y+p.y) s.(p.x).(p.y); set (p.x, p.y) "-"; p.x <- x+p.x; p.y <- y+p.y);;

let direction d = match d with
| "1" -> (-1,0)
| "2" -> (0,1)
| "3" -> (0,-1)
| "4" -> (1,0)
| _ -> (0,0);;

let border (x, y) = g |> (fun s -> if x>s.a || x<0 || y>s.b || y <0 then true else false);; 

let isEmpty (x,y) = g.map |> (fun s -> if s.(x).(y)="-" then true else false);;

let add (x,y) sym =  if (isEmpty (x,y)) = true && (border (x,y)) = false then set (x,y) sym; true;;

let stab player sym = g.map |> (function s -> let dir = (direction player.dir) in if s.(fst dir).(snd dir)=sym then set dir "-");;

dis();;

let rec addWalls nbWalls g= match nbWalls with
| 0 -> ()
| x -> if((add (Random.int g.a, Random.int g.b) "X") = true) then addWalls (x-1) g else addWalls x g;; 

addWalls 30 g;;

dis();;


let rec addFoes nbFoes g = match nbFoes with
| 0 -> ()
| x -> let a = (Random.int g.a) in let b = (Random.int g.b) in 
if ((add (a, b) "F") = true) then ((g.foes.(nbFoes-1) <- {x=a; y=b; dir="11"}); addFoes (x-1) g) else addFoes x g;;

addFoes 10 g;;

dis();;

let addPlayer g = 
let rec aux a b = 
if ((add (a, b) "P") = true) then g.character = {x=a; y=b; dir="11"}
else aux (Random.int g.a) (Random.int g.b)
in aux (Random.int g.a) (Random.int g.b);;


addPlayer g;;

dis();;





let debug_entity = 
  object(self)
    inherit entity []
  end

let a_component = 
  object(self)
  inherit component  (ref debug_entity)
  method init = print_string "initialisation"
  method update  = print_string "I'm alive"; print_newline ();
  end;;

debug_entity#add_component a_component;;


let scene = new scene [debug_entity];;

(* une image par seconde *)
let framerate = 1.;; 

let rec gameLoop (scene:scene) (last_time: float) (dt_cumulator:float) : unit= 
  let t = Sys.time() in
  let dt = (t -. last_time) in

  (* ici on limite le framerate *)
  if dt_cumulator >= framerate then (scene#sceneUpdate; gameLoop scene t 0.)
  else
  gameLoop scene (t) (dt_cumulator +. dt)
;;
gameLoop scene 0. 0.;;

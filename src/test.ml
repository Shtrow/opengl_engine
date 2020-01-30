open Engine



type player = {mutable x : int ; mutable y : int };;


type field = {map: string array array; a : int ; b : int};;


let build x y = {map=Array.make_matrix x y "-"; a=x; b=y};;


let g = build 5 7;;


let dis() = g.map |> Array.iter (fun xs -> xs|>  Array.iter (fun x -> print_string x); print_newline ());;


let set (x,y) z = g.map |> (fun s -> s.(x).(y) <- z);;

set (1,1) "€";;

let move (x, y) p = g.map |> (fun s -> 
         set (x+p.x, y+p.y) s.(p.x).(p.y); 
          set (p.x, p.y) "-";
           p.x <- x+p.x;
            p.y <- y+p.y);;



let direction d = match d with
  | "1"-> (-1,0)
    | "2"-> (0,1)
      | "3"-> (0,-1)
        | "4"-> (1, 0)
          | _-> (0,0)
;;


let border (x,y) = g |> (fun s -> if x>s.a || x<0 || y>s.b || y <0  then true else false);;

let isEmpty (x,y) = g.map|> (fun s -> if s.(x).(y)= "-" then true else false);;


Random.self_init();;

let addObj nObject sym f = 
         let rec aux obj x y = match obj with
            | 0 -> ()
               | _ -> if (isEmpty (x,y) && (border (x,y)<>false)) then 
                            ((set (x,y) sym); aux (obj-1) (Random.int f.a) (Random.int f.b))
                                 else (aux obj (Random.int f.a) (Random.int f.b))
                                   in aux nObject (Random.int f.a) (Random.int f.b);;







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

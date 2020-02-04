type player ={mutable x:int; mutable y:int; mutable dir: string};;


type field = {map:string array array; mutable character: player; a:int; b:int; foes: player array};;



Array.make 10 {x=0; y=0; dir="11"};;

let build x y = {map=Array.make_matrix x y "-"; a=x; b=y; foes= Array.make (x+y) {x=0; y=0; dir="10"} ; character = {x=0; y=0; dir="4"}};;

let g = build 5 5;;

let dis() = g.map |> Array.iter (fun xs-> xs |> Array.iter (fun x -> print_string x); print_newline ());;



let set (x,y) z = g.map |> (fun s -> s.(x).(y) <- z);;

let direction d = match d with
| "1" -> (-1,0) (*Haut*)
| "2" -> (0,1)  (*Droite*)
| "3" -> (0,-1) (*Gauche*)
| "4" -> (1,0)  (*Bas*)
| _ -> (0,0);;

let move dir p = g.map |> (fun s -> let x = (fst (direction dir)) in let y= (snd (direction dir)) in set (x+p.x, y+p.y) s.(p.x).(p.y); set (p.x, p.y) "-"; p.x <- x+p.x; p.y <- y+p.y; p.dir <- dir);;



let border (x, y) = g |> (fun s -> if x>s.a || x<0 || y>s.b || y <0 then true else false);; 

let isEmpty (x,y) = g.map |> (fun s -> if s.(x).(y)="-" then true else false);;

let add (x,y) sym =  if (isEmpty (x,y)) = true && (border (x,y)) = false then set (x,y) sym; true;;





let rec addWalls nbWalls g= match nbWalls with
| 0 -> ()
| x -> if((add (Random.int g.a, Random.int g.b) "X") = true) then addWalls (x-1) g else addWalls x g;; 




let rec addFoes nbFoes g = match nbFoes with
| 0 -> ()
| x -> let a = (Random.int g.a) in let b = (Random.int g.b) in 
if ((add (a, b) "F") = true) then ((g.foes.(nbFoes-1) <- {x=a; y=b; dir="14"}); addFoes (x-1) g) else addFoes x g;;


let addPlayer g = 
let rec aux a b = 
if ((add (a, b) "P") = true) then (g.character <- {x=a; y=b; dir="11"})
else aux (Random.int g.a) (Random.int g.b)
in aux (Random.int g.a) (Random.int g.b);;


let createWorld = g|> (fun s -> addWalls (s.a+2) s; addPlayer s; addFoes (s.a+s.b) s);;


createWorld;;
dis();;

(*A modifier*)
let stab player sym = g.map |> (function s -> let dir = (direction player.dir) in if s.(fst dir).(snd dir)=sym then print_string (s.(fst dir).(snd dir)); set dir "-");;

open Engine


(* SCENE *)
let scene1 = 
object(self)
  inherit scene [] "main"
end

(* comportement de la map *)
let e_map = 
object(self)
  inherit entity []
end
class map_extension =
  object
    val map = Array.make_matrix 10 10 '_'
    method set_case i j c = map.(i).(j) <-c
    method get_map = map
  end
type component_extension_type += B_map of map_extension;;
let c_map1 =
object(self)
  inherit component (e_map) "c_map1" ~component_extension:(B_map new map_extension)
  
  method update = 
  let map =   match self#get_component_extension with 
  |B_map m -> m
  |_ -> failwith "" in
  for i= 0 to (Array.length map#get_map)-1 do 
    print_newline();
    for j= 0 to Array.length (Array.get map#get_map i)-1 do
      print_char map#get_map.(i).(j) ; 
    done;
  done;
  print_newline();
end
;;
e_map#add_component (c_map1:>component);;
(* PLAYER *)
let player =
object(self)
  inherit actor [] 0 []
  method private perform_action  = function
      s -> 
        let action =  self#get_action s in
        action#perform
  method take_action = 
    self#perform_action (read_line ());


end

(* component fournissant un symbol *)
class b_symb=
object(self)
  val mutable symbol = 'X'
  method set_symb s = symbol <- s
  method get_symb = symbol
end
type component_extension_type += B_symbol of b_symb

let c_symb =
let p  = player in
let p = (p : actor :> entity) in
object
inherit  component p "render" ~component_extension:(B_symbol new b_symb)

end;;
(* MOVE ACTION *)
let action_move = 
object(self)
  inherit action "move" ( (player :> actor ))
  method perform = 
    begin
    let actor = self#get_actor in
    let (old_i, old_j) = actor#get_board_position in
    let direction = actor#get_direction in
    begin
      match direction with
      (* Déclancher les animations adéquates *)
      | Engine.Down -> actor#set_board_position ((old_i  ), old_j-1)
      | Engine.Up -> actor#set_board_position ((old_i ), old_j+1)
      | Engine.Right -> actor#set_board_position ((old_i +1 ), old_j)
      | Engine.Left -> actor#set_board_position ((old_i -1 ), old_j)
    end;
    let component = (actor:>entity)#get_component "render" in
    let behaviour  = (component)#get_component_extension in
    let symbol = function
        Engine.Down -> '-'
        |Engine.Up -> '^'
        |Engine.Right -> '>'
        |Engine.Left -> '<'
        in
      
    match behaviour with 
    B_symbol(b) ->
    let (new_i,new_j) = actor#get_board_position in
    let b =  match c_map1#get_component_extension with
        B_map b -> b
        |_ -> failwith""
      in b#set_case new_i new_j (symbol direction) ; 
      
      b#set_case old_i old_j '_';
    | _ -> ();
    (* print_endline "new position :";print_int old_i; print_string " "; print_int old_j;print_newline (); *)
      
    end
end;;


let action_face_right = 
object(self)
  inherit action "face right" ( (player :> actor ))
  method perform = 
    let actor = self#get_actor in
    actor#set_direction Right;
    (* let c_s = actor#get_component "symbol" in
      !c_s#set_symb '>' *)
end;;
let action_face_left = 
object(self)
  inherit action "face left" ( (player :> actor ))
  method perform = 
    let actor = self#get_actor in
    actor#set_direction Left;
end;;
let action_face_up = 
object(self)
  inherit action "face up" ( (player :> actor))
  method perform = 
    let actor = self#get_actor in
    actor#set_direction Up;
end;;
let action_face_down = 
object(self)
  inherit action "face down" ( (player :> actor))
  method perform = 
    let actor = self#get_actor in
    actor#set_direction Down;
end;;

let turn_manager = 
object(self)
  inherit entity []

end;;
let turn_manager_component actors = 
object(self)
  inherit component ( turn_manager) "turn_manager"
  method update =
    print_endline " Next Turn !";
    List.iter (fun a -> if a#is_ready then a#take_action) actors
end;;    

turn_manager#add_component (turn_manager_component [player]);;


(* (player:>entity)#add_component a_component;; *)
(player)#add_action action_face_right;;
(player)#add_action action_face_left;;
(player)#add_action action_face_down;;
(player)#add_action action_face_up;;
(player)#add_action action_move;;

scene1#add_entity (turn_manager :> entity);;
scene1#add_entity (player :> entity);;
scene1#add_entity (e_map :> entity);;
let g = new game scene1;;
g#run ();;
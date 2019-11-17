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
let c_map1 =
object(self)
  inherit component (ref e_map) "c_map1"
  val map = Array.make_matrix 10 10 '_'
  method set_char c i j = map.(i).(j) <- c
  method update = 
  for i= 0 to (Array.length map)-1 do 
    print_newline();
    for j= 0 to Array.length (Array.get map i)-1 do
      print_char map.(i).(j) ; 
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
        !action#perform
  method take_action = 
    self#perform_action (read_line ());


end
let player = ref player;;

(* component fournissant un symbol *)
(* let c_symb = 
object(self)
  inherit component (player:>(actor ref):>(entity ref)) "symbol"
  val mutable symbol = 'X'
  method set_symb s = symbol <- s
end;; *)

(* MOVE ACTION *)
let action_move = 
object(self)
  inherit action "move" (player :> actor ref)
  method perform = 
    let actor_ref = self#get_actor in
    let actor  = !actor_ref in
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
    c_map1#set_char '_' old_i old_j;
    let (new_i,new_j) = actor#get_board_position in
    c_map1#set_char 'X' new_i new_j;
    (* print_endline "new position :";print_int old_i; print_string " "; print_int old_j;print_newline (); *)
      
      
end;;


let action_face_right = 
object(self)
  inherit action "face right" ( (player :> actor ref))
  method perform = 
    let actor_ref = self#get_actor in
    let actor  = !actor_ref in
    actor#set_direction Right;
    (* let c_s = actor#get_component "symbol" in
      !c_s#set_symb '>' *)
end;;
let action_face_left = 
object(self)
  inherit action "face left" ( (player :> actor ref))
  method perform = 
    let actor_ref = self#get_actor in
    let actor  = !actor_ref in
    actor#set_direction Left;
end;;
let action_face_up = 
object(self)
  inherit action "face up" ( (player :> actor ref))
  method perform = 
    let actor_ref = self#get_actor in
    let actor  = !actor_ref in
    actor#set_direction Up;
end;;
let action_face_down = 
object(self)
  inherit action "face down" ( (player :> actor ref))
  method perform = 
    let actor_ref = self#get_actor in
    let actor  = !actor_ref in
    actor#set_direction Down;
end;;

let turn_manager = 
object(self)
  inherit entity []

end;;
let turn_manager_component actors = 
object(self)
  inherit component (ref turn_manager) "turn_manager"
  method update =
    print_endline " Next Turn !";
    List.iter (fun a -> if a#is_ready then a#take_action) actors
end;;    

turn_manager#add_component (turn_manager_component [!player]);;


(* (player:>entity)#add_component a_component;; *)
(!player)#add_action action_move;;
(!player)#add_action action_face_right;;
(!player)#add_action action_face_left;;
(!player)#add_action action_face_down;;
(!player)#add_action action_face_up;;

scene1#add_entity (turn_manager :> entity);;
scene1#add_entity (!player :> entity);;
scene1#add_entity (e_map :> entity);;
let g = new game scene1;;
g#run ();;
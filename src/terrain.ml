open Level_parser
open Engine.Render
open Engine.Core
open Math.Transform

type cell = {
  actor : actor option;
  coord : int * int;
  ground : ground;
  anim : ( animRenderer ) option
}
type terrain = cell array array;;

let map_from_ast =
  List.mapi (
    fun i l -> 
    List.mapi (
      fun j tile ->
        match tile with
        |Wall -> {
            actor = None;
            coord = (i,j);
            ground = Wall;
            anim = None;
          }
        |Glass -> {
            actor = None;
            coord = (i,j);
            ground = Glass;
            anim = None;
          }
        | _ ->
          {
            actor = None;
            coord = (i,j);
            ground = Grass;
            anim = None;
          }
    ) l
  )


let to_array l= 
  let tmp = 
  List.map Array.of_list l |> Array.of_list
  in
  tmp

let anim_map1 = 
  lazy (
  let an = new animation [ResourceManager.get_texture "map1"] false in
  new animRenderer ["map1", an]
  )

(* We make a function because we are sure that texture are load before calling get texture *)
(* unit -> animRenderer *)
let animTerrain ground = 
  let anim =
  match ground with  
    | Grass -> new animation [ResourceManager.get_texture "t_grass1";
                              ResourceManager.get_texture "t_grass2"] true
    | Wall -> new animation [ResourceManager.get_texture "wall1"] false 
    | _ -> new animation [ResourceManager.get_texture "wall1"] false 
    in
      anim#set_speed 1.0;
      new animRenderer  [ "cell",anim]

let create_terrain w h =
    let terrain = Array.init h (fun i ->
      Array.init w (fun j -> 
        {
          actor = None;
          coord = i,j;
          ground = Grass;
          anim = None
         }
      )
    ) in
    
    terrain

let map = 
  let map = 
  map_from_ast @@ Level_parser.parse "./map1" |> to_array 
  in map

let get_cell (i,j) = map.(i).(j)

(** Return true if the position (i,j) is out of boun *)
let out_of_bound (i,j) = 
  (i) >= Array.length map 
  ||(i)< 0 
  ||(j)>= Array.length map.(0)
  ||(j) < 0 

let get_actor (i,j) map =
  if out_of_bound (i,j) then None else
  map.(i).(j).actor

let move_entity e ((i,j) as v) = 
  if out_of_bound v then () else begin
  let (old_i,old_j) = e#get_position() in
  match  map.(i).(j).ground with
  | Wall | Glass-> ()
  | _ -> map.(old_i).(old_j) <- {map.(old_i).(old_j) with actor = None};
    map.(i).(j) <- {map.(i).(j) with actor = Some (e)};
    e#set_position v
    end


let is_cell_blocked ((i,j) as v) =
  out_of_bound v || 
  match map.(i).(j).ground with
  | Wall | Glass -> true
  |_  -> false

  (** Same as is_cell_blocked but no concider glass as blockable *)
let is_cell_blocked_2 ((i,j) as v) =
  out_of_bound v || 
  match map.(i).(j).ground with
  | Wall  -> true
  |_  -> false


let front_of pos dir =
  let dir = dir_to_vec dir in 
  Math.Vector2.vecI ((Math.Vector2.(+) (dir) (Math.Vector2.vecF pos)))

  (** Return the actor face to (i,j) in the direction 'direction' *)
let rec ray_cast ((i,j) as v) direction = 
  let v_f = Math.Vector2.vecF v in 
  if is_cell_blocked_2 v then None 
  else 
    match get_actor v map with
    |None -> ray_cast  (Math.Vector2.vecI (Math.Vector2.(+) (v_f)  direction)) direction
    | e -> e
  

  (** Return the cell face to (i,j) in the direction 'direction' *)
let ray_cast2 ((i,j) as v) direction = 
let rec aux prev ((i,j) as v) d = 
  let v_f = Math.Vector2.vecF v in 
  if is_cell_blocked v then get_cell prev
  else begin
    match get_actor v map with
    |None -> aux v (Math.Vector2.vecI (Math.Vector2.(+) (v_f)  d)) d
    | e -> get_cell v
    end
  in aux v ((front_of v direction)) @@ dir_to_vec direction

let exit = (1,0)

(* Creating  first terrain *)
let terrain1 =
object(self)
  inherit entity "terrain1" ()
  initializer let t = self#get_transform in self#set_transform {t with depth = -0.1 }
end

let terrainRender =
object(self)
  inherit component terrain1
  method init () = 
    (* I center the terrain *)
    let transform = self#get_entity#get_world_transform  in
    self#get_entity#set_transform @@  Math.Transform.translate transform (200.0, 100.0) ; 

    (* I assign a new render for each cells  *)
    Array.iteri ( fun i e -> 
      Array.iteri (fun j c -> 
        map.(i).(j) <-
        {c with anim = Some(animTerrain c.ground)}
        )e
    ) map

  method update () = 
    let transform =  self#get_entity#get_world_transform  in
    let transform = {transform with depth = -0.001} in 
    Array.iteri ( fun i c ->
    Array.iteri (fun j c1 ->
        let transform = {
          transform with position =
            Math.Vector2.(+) transform.position (float i *.32.0, float j *. 32.0);
          depth = -0.01
        } in 
        match c1.anim with
        | Some a -> a#draw (to_sprite_coord transform)
        | None -> ()
    ) c
   ) map
     
end
let map1Render =
  object(self)
    inherit renderComponent terrain1 anim_map1
    initializer  
    let transform = self#get_entity#get_world_transform  in
    self#get_entity#set_transform @@  Math.Transform.translate transform (200.0, 0.0) ; 
  end
;;
(terrain1:>entity)#add_component (map1Render:>component);;

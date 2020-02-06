open Engine.Render
open Engine.Core
open Math.Transform

type  ground = Grass | Wall
type cell = {
  entity : entity option;
  coord : int * int;
  ground : ground;
  anim : ( animRenderer ) option
}
type terrain = cell array array;;


(* We make a function because we are sure that texture are load before calling get texture *)
(* unit -> animRenderer *)
let animTerrain ground = 
let anim =
match ground with  
  | Grass -> new animation [ResourceManager.get_texture "t_grass1"; ResourceManager.get_texture "t_grass2"] true
  | Wall -> new animation [ResourceManager.get_texture "wall1"] false 
  in
    anim#set_speed 1.0;
    new animRenderer  [ "cell",anim]

(* TODO : More complete function create *)
let create_terrain w h =
    let terrain = Array.init h (fun i ->
      Array.init w (fun j -> 
        {
          entity = None;
          coord = i,j;
          ground = Grass;
          anim = None
         }
      )
    ) in
    
    terrain

let map = create_terrain 12 12

(* Creating  *)
let terrain =
object
  inherit entity ()
end

let terrainRender =
object(self)
  inherit component terrain
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
    Array.iteri ( fun i c ->
    Array.iteri (fun j c1 ->
        let transform = {transform with position = Math.Vector2.(+) transform.position (float i *.32.0, float j *. 32.0); depth = -0.01} in 
        (Option.get(c1.anim))#draw  (to_sprite_coord transform)
    ) c
   ) map
     
end
;;
(terrain:>entity)#add_component terrainRender;;
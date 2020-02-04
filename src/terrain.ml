open Engine.Render
open Engine.Core
open Math.Transform


let create_land w h = 
  let grass = new animation [ResourceManager.load_texture_from_file "res/grass1.png";ResourceManager.load_texture_from_file "res/grass2.png"] true in
  grass#set_speed 10.0;
  let grass = new animRenderer [ "grass",grass] in 
  Array.make_matrix w h grass

let print_land (t) l = 
  Array.iteri ( fun i c ->
    Array.iteri (fun j c1 ->
      
      c1#draw t
    ) c
   ) l


let draw_t t d = 
let t = to_sprite_coord t in 
print_land t d ;;

type case  = animation (* DIM = 16* 16 *)

(* Terraim componnent *)
let terrain =
object
  inherit entity
end

let terrainRender =
object(self)
  inherit component terrain
  val d : Engine.Render.animRenderer array array = create_land 5 5
  method init () = let transform = self#get_entity#get_world_transform  in
  self#get_entity#set_transform @@  Math.Transform.translate transform (100.0, 100.0)

  method update () = draw_t (self#get_entity#get_world_transform) d
end

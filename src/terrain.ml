open Engine.Render
open Engine.Render
open Engine.Core
open Math.Transform


let create_land w h = 
  let grass = new animation [ResourceManager.load_texture_from_file "res/grass1.png";ResourceManager.load_texture_from_file "res/grass2.png"] true in
  grass#set_speed 10.0;
  let grass = new animRenderer [ "grass",grass] in 
  Array.make_matrix w h grass

let print_land (t: spriteCoord) l = 
  Array.iteri ( fun i c ->
    Array.iteri (fun j c1 ->
      let (x,y,z) = t.position in 
      c1#draw {t with position = let (x,y) = Math.Vector2.(+)  (x,y) (float i *.32.0,float j *. 32.0) in (x,y,z)}
    ) c
   ) l


let draw_t t d = 
let t = to_sprite_coord t in 
print_land t d ;;

type case  = animation (* DIM = 16* 16 *)

let terrain =
object
  inherit entity
end

let terrainRender =
object(self)
  inherit component terrain
  val mutable d : Engine.Render.animRenderer array array = [||]
  method init () = 
   d<- (create_land 5 5);
  let transform = self#get_entity#get_world_transform  in
  self#get_entity#set_transform @@  Math.Transform.translate transform (100.0, 100.0)

  method update () = draw_t (self#get_entity#get_world_transform) d
end
;;
(terrain:>entity)#add_component terrainRender;;
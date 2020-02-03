open Engine.Render
type case  = animation (* DIM = 16* 16 *)


let create_land w h = 
  let grass = new animation [ResourceManager.load_texture_from_file "res/grass1.png";ResourceManager.load_texture_from_file "res/grass2.png"] true in
  grass#set_speed 10.0;
  let grass = new animRenderer [ "grass",grass] in 
  Array.make_matrix w h grass

let print_land  l = 
  Array.iteri ( fun i c ->
    Array.iteri (fun j c1 ->
      let t = {
        position = (float i *.32.0,float j *. 32.0, 0.0 );
        scale = 1.0,1.0;
        rotation = 1.0;
      } in
      
      c1#draw t
    ) c
   ) l
;;


let draw d = 
print_land @@ d ;;
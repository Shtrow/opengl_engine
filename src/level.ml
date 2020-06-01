open Engine.Core


let scene_completed scene =
  Actors.player#get_position () = Terrain.exit

let failed scene =
  Actors.player#is_dead

let essential_entity =
  [
    Actors.muzzle ;
    Actors.bullet ;
    Actors.blood_splash;
  ]

let lvl1 = 
lazy (

  Actors.player#set_direction North;
  Actors.player#set_position (5,18);
  let enemy1 = Actors.add_ennemy @@(new Actors.enemy1 "e1") (2,12) East in
  let enemy2 = Actors.add_ennemy @@(new Actors.enemy0 "e2") (11,5) North in
  let enemy3 = Actors.add_ennemy @@(new Actors.enemy0 "e3") (1,10) North in
  let enemy4 = Actors.add_ennemy @@(new Actors.enemy0 "e4") (3,10) North in
  let enemy5 = Actors.add_ennemy @@(new Actors.enemy0 "e5") (1,8) North in
  let enemy6 = Actors.add_ennemy @@(new Actors.enemy0 "e6") (10,4) North in
  let enemy7 = Actors.add_ennemy @@(new Actors.enemy0 "e7") (11,4) North in
  let enemy8 = Actors.add_ennemy @@(new Actors.enemy1 "e8") (8,6) East in
  let gun = Prop.gun (8,9) in 
  new scene ([
  (* Add your entities here *)
    Terrain.terrain1; 
    (enemy3:>entity);
    (enemy1:>entity);
    (enemy2:>entity);
    (enemy4:>entity);
    (enemy7:>entity);
    (enemy8:>entity);
    (enemy5:>entity);
    (enemy6:>entity);
    (gun:>entity);
    (Actors.player:>entity);
  ] @ essential_entity)
  [enemy3;enemy1;enemy2;(Actors.player:>actor); (gun:>actor);
  enemy4;
  enemy5;
  enemy6;
  enemy7;
  enemy8;
  ]
  )
  

let levels = let q = Queue.create () in

Queue.add  lvl1 q;
q

let reset () =print_endline "RESET"

let next () = 
  print_endline "NEXT LEVEL";

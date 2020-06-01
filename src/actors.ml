open Engine.Core
open Engine.Render
open Engine.Input
open Math



(* Creating animation *)
let player_anims = lazy (let d = new animation [ResourceManager.get_texture "player_idle"] true in
 let k = new animation [ResourceManager.get_texture "player_idle_knife"] false in
 let k2 = new animation [
    ResourceManager.get_texture "player_knife_attack1";
    ResourceManager.get_texture "player_knife_attack2";
    ResourceManager.get_texture "player_knife_attack3";
    ResourceManager.get_texture "player_knife_attack4";
  ] false in
 k2#set_speed 0.1;
 (new animRenderer [
   "player_idle",d;
   "player_idle_knife",k;
   "player_knife_attack",k2
 ]))

let blood_splash_anim = 
  lazy (
    let a = new animation
    [
      ResourceManager.get_texture "blood_splash1";
      ResourceManager.get_texture "blood_splash2";
      ResourceManager.get_texture "blood_splash3";
      ResourceManager.get_texture "blood_splash4";
      ResourceManager.get_texture "blood_splash5";
      ResourceManager.get_texture "blood_splash6";
      ResourceManager.get_texture "blood_splash7";
    ] false in 
    a#set_speed 0.10;
    (new animRenderer ["splash", a])
  )
let muzzle_anim = 
  lazy (let d = new animation 
  [
    ResourceManager.get_texture "muzzle1";
    ResourceManager.get_texture "muzzle2";
    ResourceManager.get_texture "muzzle3";
    ResourceManager.get_texture "muzzle4";
  ] false in
  d#set_speed 0.01;
(  new animRenderer ["fire",d]))
let enemy_anims = lazy(
let d = new animation [ResourceManager.get_texture "enemy_idle"] true in
let e = new animation [ResourceManager.get_texture "enemy_dead1"] true in

    (new animRenderer ["enemy",d;"dead", e]));;

let bullet_anim  =  lazy (let d = new animation [ResourceManager.get_texture "bullet"] false in
 (new animRenderer ["bullet",d]))

(* ACTIONS *)
let move actor =
  let v_dir = dir_to_vec @@ actor#get_direction () in 
  let (i,j) as new_p = Math.Vector2.(+) (Math.Vector2.vecF (actor#get_position()))  (v_dir) in 

  Terrain.move_entity (actor) (Math.Vector2.vecI new_p)

let faceNorth actor = 
  actor#set_direction North
let faceSouth actor = 
  actor#set_direction South
let faceEast actor = 
  actor#set_direction East
let faceWest actor = 
  actor#set_direction West


let bullet = 
object(self)
  inherit entity ~parent:Terrain.terrain1 "bullet" () 
end

let bullet_behavior = 
object(self)
  inherit component bullet
  val mutable target = 0.0,0.0
  val mutable dir = 0.0,0.0
  val bullet_speed = 1500.0
  method shoot trans pos direction =

    let t = Vector2.vecF @@ (Terrain.ray_cast2 (pos) (direction) ). coord in 
    target <-t;
    dir <-( dir_to_vec direction); 

    self#get_entity#set_transform trans
  method update () =
    let t = self#get_entity#get_transform in 

    if t.position <> (Vector2.mul_scalar 32.0 target) then begin
    
      self#get_entity#set_transform (
        Transform.translate t (Math.Vector2.mul_scalar (dt()*.bullet_speed) (dir))
      )
      end
end

let bulletRender =  
object 
inherit renderComponent bullet bullet_anim
end

let blood_splash = 
object(self)
  inherit entity ~parent:(Terrain.terrain1) "blood_splash" ()
end

let blood_splashRender = 
object
  inherit renderComponent blood_splash blood_splash_anim
end

let muzzle = 
object(self)
  inherit entity "muzzle" ()
end



let muzzleRender = 
object(self)
  inherit renderComponent muzzle muzzle_anim 
end
let muzzle_behavior= 
object(self)
  inherit component muzzle
  method init () = 
    self#get_entity#set_parent (Terrain.terrain1);
  
  method change_focus (actor:actor) = 
    let d = self#get_entity#get_transform in 
      self#get_entity#set_transform {d with depth = 0.1;
        position = Vector2.mul_scalar 32.0 
          (Vector2.vecF @@ Terrain.front_of (actor#get_position()) (actor#get_direction())) ;
        angle = actor#get_transform.angle
      }
end;;

let shoot actor =
  let a =  muzzleRender#get_render_anim ()
  in 
  muzzle_behavior#change_focus actor;
  let a = a#get_current_anim in
  a#rewind();
  bullet_behavior#shoot (actor:>entity)#get_transform 
    ((actor)#get_position()) ((actor)#get_direction());

  match 
    Terrain.ray_cast (Terrain.front_of (actor#get_position ()) (actor#get_direction()))
      ( dir_to_vec (actor#get_direction())) with 
    | None -> () 
    (* WIP : I deactivate the entity for the moment *)
    | Some ent -> print_endline "HIT" ;
    (ent)#kill true
  
let backstab actor = 
  let en =  Terrain.front_of (actor#get_position ()) (actor#get_direction()) in 
  match Terrain.get_actor en Terrain.map with
  |None -> () 
  (* Enemy ? *)
  |Some e -> 
    (blood_splashRender#get_render_anim())#get_current_anim  #rewind();
      
    (e)#kill true


let player pos dir = 
object(self)
  inherit actor ~parent:(Terrain.terrain1) "player" 0 [
    ("move",move);
    ("east",faceEast);
    ("west",faceWest);
    ("south",faceSouth);
    ("north",faceNorth);   
    ("shoot",shoot);   
    ("backstab",backstab);   
    ] pos dir as super
  val mutable ammo = 0
  method nb_ammo = ammo
  method reset () = 
    super#reset() ; ammo <- 0;
    Terrain.move_entity (self:>actor) (pos);
  method take_action () = 
      match Engine.Input.getKeyPressed () with 
        |Some( GLFW.Right) -> (self#get_action "east") (self:>actor)
        |Some( GLFW.Left) -> (self#get_action "west") (self:>actor)
        |Some( GLFW.Up) -> (self#get_action "north") (self:>actor)
        |Some( GLFW.Down) -> (self#get_action "south") (self:>actor)
        |Some (GLFW.Space) ->  
            begin
              match Terrain.get_actor (Terrain.front_of (self#get_position()) (self#get_direction())) Terrain.map with
              | Some a when (String.equal) (a:>entity)#get_tag  "gun" ->
                 self#set_state IdleGun;
                 Printf.printf "Gun picked\n";
                ammo <- (ammo+12);
                (a:>entity)#deactivate()
              | Some a when (String.equal) (a:>entity)#get_tag  "exit" ->
                  (* Next lvl*)
                ()
              |Some a -> 
                  print_endline (a:>entity)#get_tag;
                  self#set_state KnifeAttack;
                (self#get_action "backstab") (self:>actor) ;
              | _ ->
                if self# nb_ammo <=0 then 
                  self#set_state IdleKnife
                else
                 self#set_state IdleGun;
            end;
            (self#get_action "move") (self:>actor) ;

        (Option.get !scene_ref)#next_turn ()
        |Some (GLFW.R) -> 
          (Option.get !scene_ref)#reset()
        |Some (GLFW.W) -> 
          (Option.get !scene_ref)#next_turn ()
        |Some (GLFW.S) -> if self#nb_ammo > 0 then 
          begin
            ammo <- ammo -1;
            Printf.printf "Ammo : %i\n" ammo;
          (self#get_action "shoot") (self:>actor);
          let a =  muzzleRender#get_render_anim ()
          in 
          let a = a#get_current_anim in
            a#rewind();
            bullet_behavior#shoot (self:>entity)#get_transform ((self)#get_position()) ((self)#get_direction());
        (Option.get !scene_ref)#next_turn ()
          end else ()
        | _ -> ();

end

let player = player (5,18) North




(** Component that update the entity position in the terrain *)
class moveComponent actor = 
object(self)
  inherit component (actor:>entity)
  val mutable l = true
  val mutable current_position = 0.0,0.0

  method move (i, j) = 
   let old_transform = self#get_entity#get_transform in 
    self#get_entity#set_transform @@ {old_transform with position = (i *. 32., j *. 32.)}

  method init () = 
    current_position <- Math.Vector2.vecF @@ actor#get_position();
    Terrain.move_entity (actor) (actor#get_position())
    
  method update () = 
    let (dx,dy) as d =Math.Vector2.vecF @@actor#get_position() in 
    let (px,py) as p= current_position in  
    if px <> dx || py <> dy then
    begin
      let n = Math.Vector2.(-) d p |> Math.Vector2.mul_scalar (Engine.Core.dt()*.15.) in 
      current_position <- 
        Math.Vector2.(+) current_position n;
    end;
    let d = actor#get_transform in
    let angle = 
      match actor#state with 
        Aiming -> 
          let mouse_angle = Engine.Input.getMousePosition () in
          (Transform.lookAt (actor#get_world_transform) {
            d with
          position = mouse_angle;
        }) .angle       
          
        |_ ->  dir_to_angle (actor#get_direction()) in
    let d = {d with angle = angle} in 
    actor#set_transform d ; 
    self#move @@ current_position;

end

(** This component is attached to the player, and is used to control camera
 * translate zoom *)
let cameraControlComponent = 
object(self)
  inherit component (player:>entity)
  val mutable prev_mouse_pos = (0.0,0.0)
  val mutable hold_mouse_button = false
  method init () =
  let t = 
    self#get_entity#get_transform in 
    self#get_entity#set_transform {t with pivot = 16.0,16.0}
  method update () = 

    (* Camera behavior *)
    match Engine.Input.keyIsDown GLFW.F1 with 
    | true -> Camera.zoom (-0.01) 
    | _ -> ();
    if Engine.Input.isMouseButton0Down () then 
      begin
      if not hold_mouse_button then 
        begin 
          let (x, y ) as p  = getMousePosition ()  in  
          prev_mouse_pos <- p ;
          hold_mouse_button <- true ; prev_mouse_pos <-p
        end
      else
        begin
          let new_p = getMousePosition() in 
          let offset = Math.Vector2.(-) new_p prev_mouse_pos in
          Camera.move offset ;
          prev_mouse_pos <- new_p;
        end
      end
    else hold_mouse_button <- false
end

let playerRender = 
object(self)
        (* Note that we use Core.renderCompenent here *)
inherit renderComponent (player:>entity) (player_anims )
end;;

let player_anim_script render = 
object(self)
  inherit component (player:>entity)
  method update () =
    let t = (self#get_entity#get_transform) in 
    player#set_transform {t with depth = 0.01};
    let b_pos = (Vector2.(+) (Engine.Core.dir_to_vec @@ (player:> actor) # get_direction()
    |> Vector2.mul_scalar 16.00
    ) t.position) 
    in 
      blood_splash#set_transform {
        t with angle = (player:>entity)#get_transform.angle; depth = 0.2; position = b_pos
      };
match player#state with
    | IdleKnife -> 
      ((render())#get_current_anim #rewind)();
      (render())#set_animation "player_idle_knife";
    | KnifeAttack -> 
      (render())#set_animation "player_knife_attack";
    | IdleGun -> 
      ((render())#get_current_anim #rewind)();
      (render())#set_animation "player_idle";
    | Aiming ->
        ()
end


  
(* Enemies *)
class enemy0 tag pos dir= 
object(self)
  inherit actor ~parent:(Terrain.terrain1) tag 1 [
    ("move",move);
    ("east",faceEast);
    ("west",faceWest);
    ("south",faceSouth);
    ("north",faceNorth); 
    ("shoot",shoot); 
  ] pos dir as super
  method reset () = 
    super#reset() ;
    Terrain.move_entity (self:>actor) (pos);
  method is_ready () =
    is_ready
  method take_action () =

    begin
    let dir = dir_to_vec (self#get_direction()) in 
    (* If an enemy meet the player, he shoot him  *)
      match Terrain.ray_cast  (Terrain.front_of position (self#get_direction())) (dir) with 
        | None -> ()
        |Some e-> 
              if String.equal e#get_tag "player" then 
                (self#get_action "shoot") (self:>actor)
    end;
    is_ready <- false
end;;


class enemy1 tag pos dir = 
object(self)
  inherit enemy0 tag pos dir as super
  method is_ready () =
    is_ready
  method take_action () =
    begin
    match Terrain.is_cell_blocked @@ 
      (Vector2.(+) (Vector2.vecF position ) (dir_to_vec (self#get_direction()) ) |> Vector2.vecI) 
    with
    | false -> (self#get_action "move") (self:>actor) ;
    | _ -> self#set_direction (back (self#get_direction()))
    end;
    super#take_action()
end;;




class enemyRender enemy= 
object(self)
        (* Note that we use Core.renderCompenent here *)
inherit renderComponent (enemy:>entity) (enemy_anims )

end
class dead_behavior render actor enemy = 
object(self)
  inherit component (enemy:>entity)
  val r = render
  val a = actor
  method init() = 

    (actor:>entity)#add_component (self:>component)
    

  method update () = 
    if actor#is_dead then begin(render())#set_animation "dead";
    let t = (actor:>entity)#get_transform in 
    (actor:>entity)#set_transform 
      {t with  depth = 0.001; angle = (player:>actor)#get_transform.angle }
    end
    else 
    (render())#set_animation "enemy";
end

let add_ennemy enemy =
  let render = new enemyRender enemy in 
  let c2 = new moveComponent enemy in 
  let c1 = new dead_behavior render#get_render_anim enemy enemy in 
  (enemy:>entity)#add_component (render:>component);
  (enemy:>entity)#add_component (c2 :>component);
  (enemy:>entity)#add_component (c1 :>component);
  enemy
;;

(player:>entity)#add_component (playerRender:>component);;
(player:>entity)#add_component (player_anim_script playerRender#get_render_anim:>component);;
(player:>entity)#add_component (new moveComponent (player:>actor) :>component);;
(player:>entity)#add_component (cameraControlComponent:>component);;

(muzzle:>entity)#add_component (muzzleRender:>component);;
(muzzle:>entity)#add_component (muzzle_behavior:>component);;

(blood_splash:>entity)#add_component (blood_splashRender:>component);;

(bullet:>entity)#add_component (bulletRender:>component);;
(bullet:>entity)#add_component (bullet_behavior:>component);;

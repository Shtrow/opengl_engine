type texture2D
type spriteCoord = 
{
  position : float*float*float;
  scale : float*float;
  rotation : float;
}
type window =  GLFW.window

val dt : unit -> float
val ref_dt: float ref

val width : int
val height : int

val to_sprite_coord : Math.Transform.transform -> spriteCoord

module ResourceManager : 
sig
(* create a texture accessible by "get_texture name"  *)
  val load_texture_from_file : name:string -> path:string -> unit
  val load_textures : unit -> unit
  val get_texture : string -> texture2D
end
module SpriteRenderer : 
sig
  (* texture -> screen relative position -> size in pixel -> angle in degree -> color (white is [|1.0,1.0,1.0,1.0|]) *)
  val drawSprite : texture2D:texture2D -> position:(float*float*float) -> size:(float * float) -> angle:float -> color:float array -> unit
end

module Camera :
sig
  val zoom : float -> unit
  val move : (float*float) -> unit
end

(* texture list -> loop? -> *)
class animation : texture2D list -> bool ->
object
  val textures : texture2D list
  val mutable texturesFlow : texture2D list
  val mutable speed : float
  val mutable counter : float
  val color : float array
  val loop :bool
  method is_finished : unit -> bool
  method rewind : unit -> unit
  method get_color : unit -> float array
  method set_speed : float -> unit
  method drawCurrentFrame : spriteCoord -> unit
end

(* Package of animation. Choose which one will be displayed with set_animation *)
class animRenderer :  (string* animation) list ->
object
  val animations : (string * animation) list
  (* Change current animation from here *)
  method set_animation : string -> unit
  method get_animation : string -> animation
  (* This update will draw current animation *)
  method draw : spriteCoord -> unit 

end

val init_graphic : unit -> window
val update_graphic : window ->unit
val clear : unit -> unit
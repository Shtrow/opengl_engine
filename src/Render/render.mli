type texture2D
type transform
type window

val width : int
val height : int

module ResourceManager : 
sig
  val load_texture_from_file : string -> texture2D
end
module SpriteRenderer : 
sig
  (* texture -> screen relative position -> size in pixel -> angle in degree -> color (white is [|1.0,1.0,1.0,1.0|]) *)
  val drawSprite : texture2D:texture2D -> position:(float*float*float) -> size:(float * float) -> angle:float -> color:float array -> unit
end

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
  method drawCurrentFrame : transform -> unit
end


class animRenderer : (string* animation) list ->
object
  val animations : (string * animation) list
  val mutable currentAnimation : animation
  method set_animation : string -> unit
  method draw : transform -> unit
end

val init_graphic : unit -> window
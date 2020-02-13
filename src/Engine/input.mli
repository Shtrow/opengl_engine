
type key_code = GLFW.key
val window_in_for_input : GLFW.window -> unit
val init_input_callback : GLFW.window -> unit
val scroll_offset : float ref


(* True while the key is pressed *)
val keyIsDown : key_code -> bool

(* True during the frame when the key is pressed *)
val keyIsPressed : key_code -> bool

(* True during the frame when the key is released *)
val keyIsReleased : key_code -> bool

(* Return the key currently pressed or None *)
val getKeyPressed : unit -> key_code option

val getMousePosition :unit -> (float*float)

val isMouseButton0Down : unit -> bool
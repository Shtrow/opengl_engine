
type key_code = GLFW.key
val window_in_for_input : GLFW.window -> unit

(* True while the key is pressed *)
val keyIsDown : key_code -> bool

(* True during the frame when the key is pressed *)
val keyIsPressed : key_code -> bool

(* True during the frame when the key is released *)
val keyIsReleased : key_code -> bool


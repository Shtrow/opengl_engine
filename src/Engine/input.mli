
type key_code = GLFW.key
(* True while the key is pressed *)
val keyIsDown : Render.window -> key_code -> bool

(* True during the frame when the key is pressed *)
val keyIsPressed : key_code -> bool

(* True during the frame when the key is released *)
val keyIsReleased : key_code -> bool


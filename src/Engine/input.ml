type key_code = GLFW.key

open GLFW

let window : GLFW.window option ref = ref None
let window_in_for_input w = window := Some w
let key_pressed_buf = ref []
let scroll_offset = ref 0.0

let w() = (Option.get !window)

let keyIsDown k = GLFW.getKey~window:(w()) ~key:k


let keyIsPressed k = 
  true

let keyIsReleased k = true

let isMouseButton0Down () = getMouseButton (w()) 0

let isMouseButton1Down () = getMouseButton (w()) 1

let getKeyPressed () = 
  match !key_pressed_buf with 
  |[] -> None
  |hd::t ->  key_pressed_buf := t; Some hd

(* open GLFW in  *)

let getMousePosition () = GLFW.getCursorPos (w())

let func_key window (key:key) scancode (action:key_action) mods =
    match action with
| GLFW.Release ->  ()
| GLFW.Press -> key_pressed_buf := key::!(key_pressed_buf)
| GLFW.Repeat ->  ()

let func_scroll window xoffset yoffset = 
  ()
  (* print_float xoffset *)
  (* scroll_offset := xoffset *)

let func_mouse window i b key_mod_l = 
  ()

let init_input_callback window=
  GLFW.setKeyCallback window (Some func_key ) |>ignore;
  GLFW.setMouseButtonCallback window (Some func_mouse) |>ignore;
  (* GLFW.setScrollCallback window (Some func_scroll) |>ignore; *)
  
  

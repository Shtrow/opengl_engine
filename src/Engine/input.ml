type key_code = GLFW.key

open GLFW

let window : GLFW.window option ref = ref None
let window_in_for_input w = window := Some w
let key_pressed_buf = ref []

let w() = (Option.get !window)

let keyIsDown k = GLFW.getKey~window:(w()) ~key:k


let keyIsPressed k = 
  true

let keyIsReleased k = true

let getKeyPressed () = 
  match !key_pressed_buf with 
  |[] -> None
  |hd::t ->  key_pressed_buf := t; Some hd

(* open GLFW in  *)

let func_key window (key:key) scancode (action:key_action) mods =
    match action with
| GLFW.Release ->  ()
| GLFW.Press -> key_pressed_buf := key::!(key_pressed_buf)
| GLFW.Repeat ->  ()


let init_input_callback window=
  GLFW.setKeyCallback window (Some func_key ) |>ignore;;
  
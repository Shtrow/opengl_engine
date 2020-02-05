type key_code = GLFW.key
let window : GLFW.window option ref = ref None
let window_in_for_input w = window := Some w

let w() = (Option.get !window)

let keyIsDown k = GLFW.getKey~window:(w()) ~key:k

let keyIsPressed k = true

let keyIsReleased k = true


type key_code = GLFW.key

let keyIsDown window k = GLFW.getKey~window:window ~key:k

let keyIsPressed k = true

let keyIsReleased k = true


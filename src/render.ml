(* let create_window h w =
  Sdl.init Sdl.Init.everything |> ignore ;
  Sdl.init Sdl.Init.video |> ignore ;
  let window = Sdl.create_window "My Game" ~h:h ~w:w (Sdl.Window.(+) Sdl.Window.shown Sdl.Window.opengl) |> get_result in
  (* let window = Sdl.Window.create ~title:"My Game" ~dims:(h,w) in  *)
  let event = Sdl.Event.create () in

  let gl_context = Sdl.gl_create_context window |> get_result in 


  let () = 
  create_window 600 800;; *)

open GL
open VertArray
open VBO
open Tsdl
open GLFW
(* open Glu *)
open Glut

let height = 600;;
let width = 800;;




(* VERTEX SHADER *)
let vertexShaderSource = "
#version 330 core
in vec3 aPos;
in vec4 myColour;
in vec2 texturePos;

out vec4 colour;
out vec2 texCoord;

uniform float x_offset;
void main()
{
    colour = vec4(aPos,0.1);
    gl_Position = vec4(aPos.x+x_offset, -aPos.y, aPos.z, 1.0);
    texCoord = texturePos;
}";;

(* FRAGMENT SHADER *)
let fragmentShaderSource = "
#version 330 core
in vec4 colour;
in vec2 texCoord;

out vec4 FragColor;

uniform sampler2D ourTexture;

void main()
{
    FragColor = texture(ourTexture,texCoord);
} ";;

init();;
windowHint ~hint:ContextVersionMajor ~value:3;;
windowHint ~hint:ContextVersionMinor ~value:3;;
windowHint ~hint:OpenGLProfile ~value:CoreProfile;;

let window = GLFW.createWindow ~width:width ~height:height ~title:"My Game" ();;
let frame_buffer_size_callback = fun window width height -> glViewport ~x:0 ~y:0 ~width:width ~height:height;;

makeContextCurrent ~window:(Some window) ;;
setFramebufferSizeCallback ~window:window ~f:(Option.Some(frame_buffer_size_callback));;

glViewport ~x:0 ~y:0 ~height:height ~width:width;;

let vertexShader = glCreateShader ~shader_type:GL_VERTEX_SHADER;;
glShaderSource ~shader:vertexShader vertexShaderSource;;
glCompileShader ~shader:vertexShader;;
if glGetShaderCompileStatus ~shader:vertexShader then () else print_endline "vertex shader compilation failed";;
print_endline (glGetShaderInfoLog ~shader:vertexShader);;

let fragmentShader = glCreateShader ~shader_type:GL_FRAGMENT_SHADER;;
glShaderSource ~shader:fragmentShader fragmentShaderSource;;
glCompileShader ~shader:fragmentShader;;
if glGetShaderCompileStatus ~shader:fragmentShader then () else print_endline "fragment shader compilation failed";;

(* shaderProgram, which is the linking of all shaders previously defined*)
let shaderProgram = glCreateProgram();;
glAttachShader ~program:shaderProgram ~shader:vertexShader;;
glAttachShader ~program:shaderProgram ~shader:fragmentShader;;
glLinkProgram ~program:shaderProgram;;
glUseProgram ~program:shaderProgram;;
(* deleting shaders, dont need anymore *)
glDeleteShader~shader:vertexShader;;
glDeleteShader~shader:fragmentShader;;

(* let triangle_verticies = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 9;;
triangle_verticies.{0}<- -0.5;;
triangle_verticies.{1}<- -0.5;;
triangle_verticies.{2}<- 0.0;;
triangle_verticies.{3}<- 0.5;;
triangle_verticies.{4}<- -0.5;;
triangle_verticies.{5}<- 0.0;;
triangle_verticies.{6}<- 0.0;;
triangle_verticies.{7}<- 0.5;;
triangle_verticies.{8}<- 0.0;; *)

let triangle_verticies = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout [|
  (* position *)    (* texture coords *)
  0.5; 0.5; 0.0;      1.0;1.0;
  0.5; -0.5; 0.0;     1.0;0.0;
  -0.5; -0.5; 0.0;    0.0;0.0;
  -0.5; 0.5; 0.0;     0.0;1.0;
|];;
let indices = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout (
    Array.map Int32.of_int [|
  0;1;3;
  1;2;3
|]);;

(* TEXTURE *)

let texture_id = glGenTexture ();;
glBindTexture2D texture_id;;
glTexParameter GL_TEXTURE_2D  (TexParam.GL_TEXTURE_MAG_FILTER  Mag.GL_NEAREST);;
glTexParameter GL_TEXTURE_2D  (TexParam.GL_TEXTURE_MIN_FILTER  Min.GL_NEAREST);;
glTexParameter GL_TEXTURE_2D  (TexParam.GL_TEXTURE_WRAP_S  GL_REPEAT);;
glTexParameter GL_TEXTURE_2D  (TexParam.GL_TEXTURE_WRAP_T  GL_REPEAT);;
(* todo : generate mipmap *)
(* Loading image *)
let filename = "res/container.jpg";;
let texture, t_width, t_height, internal_format, pixel_data_format = Jpeg_loader.load_img (Filename filename);;
glTexImage2D GL_TEXTURE_2D 0 GL_RGB t_width t_height GL_RGB GL_UNSIGNED_BYTE texture;;


let processIntput window = 
  match getKey~window:window ~key:Escape with
  | true -> setWindowShouldClose ~window:window ~b:true
  |_ -> ();;

let vao = glGenVertexArray ();;
let vbo = glGenBuffer ();;
let ebo = glGenBuffer ();;

glBindVertexArray vao;;

glBindBuffer ~target:GL_ARRAY_BUFFER ~vbo:vbo;;
glBufferData ~target: GL_ARRAY_BUFFER ~size:( ba_sizeof triangle_verticies) ~usage:GL_STATIC_DRAW ~data: triangle_verticies;;

glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo;
glBufferData GL_ELEMENT_ARRAY_BUFFER (ba_sizeof indices) indices GL_STATIC_DRAW;;


let vertexPositionAttrib = glGetAttribLocation shaderProgram "aPos";;
let vertexColorAttrib = glGetAttribLocation shaderProgram "myColour" ;;
let vertexTextPosAttrib = glGetAttribLocation shaderProgram "texturePos" ;;




(* AttribPointer *)
glVertexAttribPointerOfs32 vertexPositionAttrib 3 GL_FLOAT false 5 0 ;; 
glEnableVertexAttribArray vertexPositionAttrib;;
glVertexAttribPointerOfs32 vertexTextPosAttrib 2 GL_FLOAT false 5 3;;
glEnableVertexAttribArray vertexTextPosAttrib;;
(* glVertexAttribPointerOfs32 vertexColorAttrib 3 GL_FLOAT false 6 3;;
glEnableVertexAttribArray vertexColorAttrib;; *)
(* unbind ? *)
(* glBindBuffer ~target:GL_ARRAY_BUFFER ;; *)

(* unbind vao, cause we are done with it *)
(* glPolygonMode GL_FRONT_AND_BACK GL_LINE ;; *)
glBindVertexArray 0;;

let loop = 

while (not (GLFW.windowShouldClose ~window:window)) do 
  (* input *)
  processIntput window;

  (* rendering *)
  glClearColor ~r:0.2 ~g:0.3 ~b:0.3 ~a:1.0;
  glClear ~mask: [GL_COLOR_BUFFER_BIT];

  let offset_attrib_location = glGetUniformLocation shaderProgram "x_offset" in
  glUniform1f offset_attrib_location 0.0 ;

  (* drawing triangle *)
  glUseProgram shaderProgram ;
  (* glBindTexture2D texture_id; *)
  glBindVertexArray vao;
  glDrawElements0 GL_TRIANGLES (Bigarray.Array1.dim indices) GL_UNSIGNED_INT ;
  (* glDrawArrays ~mode:GL_TRIANGLES ~first:0 ~count:4; *)


  swapBuffers ~window:window;
  pollEvents();
done;
glDeleteVertexArray vao ;
glDeleteBuffer ~vbo:vbo;
terminate();
;;
loop;;
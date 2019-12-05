module Matrix = 
struct
type vector2 = float*float*float
type matrix = float array

let identity  =
  [| 1.0; 0.0; 0.0; 0.0;
     0.0; 1.0; 0.0; 0.0;
     0.0; 0.0; 1.0; 0.0;
     0.0; 0.0; 0.0; 1.0; |]


let translation_matrix (x,y,z) =
  [| 1.0; 0.0; 0.0; 0.0;
     0.0; 1.0; 0.0; 0.0;
     0.0; 0.0; 1.0; 0.0;
       x;   y;   z; 1.0; |]


let normalise_vector (x,y,z) : vector2 =
  let len = sqrt(x *. x +. y *. y +. z *. z) in
  (x /. len, y /. len, z /. len)


let rotation_matrix_of_axis ~dir ~angle =
  let angle = angle *. 0.5 in
  let vn_x, vn_y, vn_z = normalise_vector dir in
  let sinAngle = sin angle in
  let qx = vn_x *. sinAngle
  and qy = vn_y *. sinAngle
  and qz = vn_z *. sinAngle
  and qw = cos angle
  in
  let x2 = qx *. qx
  and y2 = qy *. qy
  and z2 = qz *. qz
  and xy = qx *. qy
  and xz = qx *. qz
  and yz = qy *. qz
  and wx = qw *. qx
  and wy = qw *. qy
  and wz = qw *. qz in
  [| 1.0 -. 2.0 *. (y2 +. z2) ; 2.0 *. (xy -. wz);  2.0 *. (xz +. wy); 0.0;
     2.0 *. (xy +. wz);  1.0 -. 2.0 *. (x2 +. z2);  2.0 *. (yz -. wx); 0.0;
     2.0 *. (xz -. wy);  2.0 *. (yz +. wx);  1.0 -. 2.0 *. (x2 +. y2); 0.0;
     0.0;  0.0;  0.0;  1.0; |]

let mult_matrix ~m1 ~m2 =
  if Array.length m1 <> 16
  || Array.length m2 <> 16
  then invalid_arg "mult_matrix";

  let mat1_get = Array.unsafe_get m1
  and mat2_get = Array.unsafe_get m2 in

  let m1_0  = mat1_get 0     and m2_0  = mat2_get 0
  and m1_1  = mat1_get 1     and m2_1  = mat2_get 1
  and m1_2  = mat1_get 2     and m2_2  = mat2_get 2
  and m1_3  = mat1_get 3     and m2_3  = mat2_get 3
  and m1_4  = mat1_get 4     and m2_4  = mat2_get 4
  and m1_5  = mat1_get 5     and m2_5  = mat2_get 5
  and m1_6  = mat1_get 6     and m2_6  = mat2_get 6
  and m1_7  = mat1_get 7     and m2_7  = mat2_get 7
  and m1_8  = mat1_get 8     and m2_8  = mat2_get 8
  and m1_9  = mat1_get 9     and m2_9  = mat2_get 9
  and m1_10 = mat1_get 10    and m2_10 = mat2_get 10
  and m1_11 = mat1_get 11    and m2_11 = mat2_get 11
  and m1_12 = mat1_get 12    and m2_12 = mat2_get 12
  and m1_13 = mat1_get 13    and m2_13 = mat2_get 13
  and m1_14 = mat1_get 14    and m2_14 = mat2_get 14
  and m1_15 = mat1_get 15    and m2_15 = mat2_get 15
  in
  [|
    m1_0 *. m2_0  +. m1_4 *. m2_1  +. m1_8  *. m2_2  +. m1_12 *. m2_3;
    m1_1 *. m2_0  +. m1_5 *. m2_1  +. m1_9  *. m2_2  +. m1_13 *. m2_3;
    m1_2 *. m2_0  +. m1_6 *. m2_1  +. m1_10 *. m2_2  +. m1_14 *. m2_3;
    m1_3 *. m2_0  +. m1_7 *. m2_1  +. m1_11 *. m2_2  +. m1_15 *. m2_3;
    m1_0 *. m2_4  +. m1_4 *. m2_5  +. m1_8  *. m2_6  +. m1_12 *. m2_7;
    m1_1 *. m2_4  +. m1_5 *. m2_5  +. m1_9  *. m2_6  +. m1_13 *. m2_7;
    m1_2 *. m2_4  +. m1_6 *. m2_5  +. m1_10 *. m2_6  +. m1_14 *. m2_7;
    m1_3 *. m2_4  +. m1_7 *. m2_5  +. m1_11 *. m2_6  +. m1_15 *. m2_7;
    m1_0 *. m2_8  +. m1_4 *. m2_9  +. m1_8  *. m2_10 +. m1_12 *. m2_11;
    m1_1 *. m2_8  +. m1_5 *. m2_9  +. m1_9  *. m2_10 +. m1_13 *. m2_11;
    m1_2 *. m2_8  +. m1_6 *. m2_9  +. m1_10 *. m2_10 +. m1_14 *. m2_11;
    m1_3 *. m2_8  +. m1_7 *. m2_9  +. m1_11 *. m2_10 +. m1_15 *. m2_11;
    m1_0 *. m2_12 +. m1_4 *. m2_13 +. m1_8  *. m2_14 +. m1_12 *. m2_15;
    m1_1 *. m2_12 +. m1_5 *. m2_13 +. m1_9  *. m2_14 +. m1_13 *. m2_15;
    m1_2 *. m2_12 +. m1_6 *. m2_13 +. m1_10 *. m2_14 +. m1_14 *. m2_15;
    m1_3 *. m2_12 +. m1_7 *. m2_13 +. m1_11 *. m2_14 +. m1_15 *. m2_15;
  |]
let perspective_projection ~fov ~ratio ~near ~far =

  let pi = 3.14159265358979323846 in
  let maxY = near *. tan (fov *. pi /. 360.0) in
  let minY = -. maxY in
  let minX = minY *. ratio
  and maxX = maxY *. ratio in

  let x_diff = maxX -. minX in
  let y_diff = maxY -. minY in
  let z_diff = far -. near in
  let near_twice = 2.0 *. near in

  let a = near_twice /. x_diff
  and b = near_twice /. y_diff
  and c = (maxX +. minX) /. x_diff
  and d = (maxY +. minY) /. y_diff
  and e = -. (far +. near) /. z_diff
  and f = -. (near_twice *. far) /. z_diff
  in
  [| a;   0.0; 0.0; 0.0;
     0.0; b;   0.0; 0.0;
     c;   d;   e;  -1.0;
     0.0; 0.0; f;   0.0; |]

let rotate dir angle m = 
  mult_matrix  (rotation_matrix_of_axis dir angle ) (m) 

let translate (x,y,z) m= 
  let t_m = translation_matrix (x,y,z) in
  mult_matrix m t_m

end;;
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

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main()
{
    colour = vec4(aPos,0.1);
    gl_Position = projection * view * model * vec4(aPos, 1.0);
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
let rad deg = Float.pi*.180.0 *. deg;;






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

let cube_verticies = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout [|

    (* triangle coord *)(** texture coord *)
    -0.5; -0.5; -0.5;  0.0; 0.0;
     0.5; -0.5; -0.5;  1.0; 0.0;
     0.5;  0.5; -0.5;  1.0; 1.0;
     0.5;  0.5; -0.5;  1.0; 1.0;
    -0.5;  0.5; -0.5;  0.0; 1.0;
    -0.5; -0.5; -0.5;  0.0; 0.0;

    -0.5; -0.5;  0.5;  0.0; 0.0;
     0.5; -0.5;  0.5;  1.0; 0.0;
     0.5;  0.5;  0.5;  1.0; 1.0;
     0.5;  0.5;  0.5;  1.0; 1.0;
    -0.5;  0.5;  0.5;  0.0; 1.0;
    -0.5; -0.5;  0.5;  0.0; 0.0;

    -0.5;  0.5;  0.5;  1.0; 0.0;
    -0.5;  0.5; -0.5;  1.0; 1.0;
    -0.5; -0.5; -0.5;  0.0; 1.0;
    -0.5; -0.5; -0.5;  0.0; 1.0;
    -0.5; -0.5;  0.5;  0.0; 0.0;
    -0.5;  0.5;  0.5;  1.0; 0.0;

     0.5;  0.5;  0.5;  1.0; 0.0;
     0.5;  0.5; -0.5;  1.0; 1.0;
     0.5; -0.5; -0.5;  0.0; 1.0;
     0.5; -0.5; -0.5;  0.0; 1.0;
     0.5; -0.5;  0.5;  0.0; 0.0;
     0.5;  0.5;  0.5;  1.0; 0.0;

    -0.5; -0.5; -0.5;  0.0; 1.0;
     0.5; -0.5; -0.5;  1.0; 1.0;
     0.5; -0.5;  0.5;  1.0; 0.0;
     0.5; -0.5;  0.5;  1.0; 0.0;
    -0.5; -0.5;  0.5;  0.0; 0.0;
    -0.5; -0.5; -0.5;  0.0; 1.0;

    -0.5;  0.5; -0.5;  0.0; 1.0;
     0.5;  0.5; -0.5;  1.0; 1.0;
     0.5;  0.5;  0.5;  1.0; 0.0;
     0.5;  0.5;  0.5;  1.0; 0.0;
    -0.5;  0.5;  0.5;  0.0; 0.0;
    -0.5;  0.5; -0.5;  0.0; 1.0
|];;
let triangle_verticies = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout [|
  (* position *)    (* texture coords *)
  0.5; 0.5; 0.0;      1.0;1.0;
  0.5; -0.5; 0.0;     1.0;0.0;
  -0.5; -0.5; 0.0;    0.0;0.0;
  -0.5; 0.5; 0.0;     0.0;1.0;
|];;
let triangle_indices = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout (
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
glBufferData ~target: GL_ARRAY_BUFFER ~size:( ba_sizeof cube_verticies) ~usage:GL_STATIC_DRAW ~data: cube_verticies;;

glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo;
glBufferData GL_ELEMENT_ARRAY_BUFFER (ba_sizeof triangle_indices) triangle_indices GL_STATIC_DRAW;;


let vertexPositionAttrib = glGetAttribLocation shaderProgram "aPos";;
let vertexColorAttrib = glGetAttribLocation shaderProgram "myColour" ;;
let vertexTextPosAttrib = glGetAttribLocation shaderProgram "texturePos" ;;

let modelMatrixUnif = glGetUniformLocation shaderProgram "model" ;;
let viewMatrixUnif = glGetUniformLocation shaderProgram "view" ;;
let projectionMatrixUnif = glGetUniformLocation shaderProgram "projection" ;;


(* Matrix test *)
let tranform_matrix =  (Matrix.rotation_matrix_of_axis (0.0,0.0,1.0) 1.57) ;;

let model_matrix = Matrix.rotation_matrix_of_axis  (1.0,0.0,0.0) (rad (-. 55.0));;

let view_matrix =  Matrix.translation_matrix (0.0,0.0,-3.0);;

let perspective_projection_matrix = Matrix.perspective_projection (45.0) (float_of_int width/. float_of_int height) 0.1 100.0;; 

(* Multiple cube *)
let cube_position = 
(* 10 cube positions *)
[
  (0.0,0.0,0.0);
  (2.0,5.0,-15.0);
  (2.0,5.0,-15.0);
  (-1.5, -2.2, -2.5);  
  (-3.8, -2.0, -12.3);  
  ( 2.4, -0.4, -3.5);  
  (-1.7,  3.0, -7.5);  
  ( 1.3, -2.0, -2.5);  
  ( 1.5,  2.0, -2.5); 
  ( 1.5,  0.2, -1.5); 
  (-1.3,  1.0, -1.5) 


 ];;

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
glEnable GL_DEPTH_TEST;;

let loop = 

while (not (GLFW.windowShouldClose ~window:window)) do 
  (* input *)
  processIntput window;

  (* rendering *)
  glClearColor ~r:0.2 ~g:0.3 ~b:0.3 ~a:1.0;
  glClear ~mask: [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];
  glEnable (GL_DEPTH_TEST);  
  let offset_attrib_location = glGetUniformLocation shaderProgram "x_offset" in
  glUniform1f offset_attrib_location 0.0 ;

  

  (* drawing triangle *)
  glUseProgram shaderProgram ;
  (* glBindTexture2D texture_id; *)
  glBindVertexArray vao;

  (* vertex Tranformation *)

  
  glUniformMatrix4fv viewMatrixUnif 1 false view_matrix;
  glUniformMatrix4fv projectionMatrixUnif 1 false perspective_projection_matrix;

  List.iter
  (
    fun position->
      let new_model_matrix = Matrix.translation_matrix position |> Matrix.rotate (2.0,1.3,1.5) (rad 20.0 ) in

      glUniformMatrix4fv modelMatrixUnif 1 false new_model_matrix;
      glDrawArrays ~mode:GL_TRIANGLES ~first:0 ~count:36;
      
  )
  cube_position;

  (* glDrawElements0 GL_TRIANGLES (Bigarray.Array1.dim triangle_indices) GL_UNSIGNED_INT ; *)


  swapBuffers ~window:window;
  pollEvents();
done;
glDeleteVertexArray vao ;
glDeleteBuffer ~vbo:vbo;
terminate();
;;
loop;;
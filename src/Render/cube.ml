open GL          (* the base functions of OpenGL *)
open VertArray   (* Vertex-Array, needed by VBO, VBO are build on top of VA's *)
open VBO         (* Vertex Buffer Object, the most efficient drawing method
                      and the base drawing method in OpenGL 3.X *)
open Glut        (* windowing with Glut *)


let msecs = 5000  (* print fps every 5 seconds *)

(* product of the modelview (world) matrix and the projection matrix *)
let modelviewProjectionMatrix = ref [| |]


let cube_vertices =
  Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout [|
    (* RGB colors *)  (* XYZ coords *)
    0.0; 1.0; 0.0;    -1.0;  1.0; -1.0;
    0.0; 0.0; 0.0;    -1.0; -1.0; -1.0;
    1.0; 1.0; 0.0;    -1.0;  1.0;  1.0;
    1.0; 0.0; 0.0;    -1.0; -1.0;  1.0;
    1.0; 1.0; 1.0;     1.0;  1.0;  1.0;
    1.0; 0.0; 1.0;     1.0; -1.0;  1.0;
    0.0; 1.0; 1.0;     1.0;  1.0; -1.0;
    0.0; 0.0; 1.0;     1.0; -1.0; -1.0;
  |]

let cube_indices =
  Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout (
    Array.map Int32.of_int [|
      (* 6 squares, each square made of 2 triangles,
         quad faces don't exist anymore in OGL 3.X *)
      0;1;3;  3;2;0;
      4;5;7;  7;6;4;
      3;1;7;  7;5;3;
      0;2;4;  4;6;0;
      6;7;1;  1;0;6;
      2;3;5;  5;4;2;
    |]
  )


(* construct a projection matrix *)
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


let translation_matrix (x,y,z) =
  [| 1.0; 0.0; 0.0; 0.0;
     0.0; 1.0; 0.0; 0.0;
     0.0; 0.0; 1.0; 0.0;
       x;   y;   z; 1.0; |]


(* multiply two matrices *)
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


let reshape ~width ~height =
  let height = max height 1 in
  glViewport 0 0 width height;
  let ratio = float width /. float height in

  (* creation of the matrices *)
  let projectionMatrix = perspective_projection 56.0 ratio 1.0 500.0 in
  let worldMatrix = translation_matrix (0.0, 0.0, -6.0) in
  modelviewProjectionMatrix := mult_matrix projectionMatrix worldMatrix;
;;


let normalise_vector (x,y,z) =
  let len = sqrt(x *. x +. y *. y +. z *. z) in
  (x /. len, y /. len, z /. len)

(* create a rotation matrix defined by a rotation axis and a rotation angle *)
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


let frame_count = ref 0

let display
      (mesh_buffers, ndx_len,
       (shader_prog,
        uniformID,
        vertexPositionAttrib,
        vertexColorAttrib, _, _)) = function () ->

  glClear [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];

  let now = Unix.gettimeofday() in
  let y = cos now
  and z = sin now in
  let dir = (0.0, y, z)        (* this is the axis of the rotation *)
  and angle = (now *. 0.8) in  (* and this is the angle of the rotation *)

  let rot_mat = rotation_matrix_of_axis dir angle in
  let world_proj_matrix = mult_matrix !modelviewProjectionMatrix rot_mat in

  glUseProgram shader_prog;
  glUniformMatrix4fv uniformID 1 false world_proj_matrix;

  (* activate the 2 generic arrays *)
  glEnableVertexAttribArray vertexColorAttrib;
  glEnableVertexAttribArray vertexPositionAttrib;

  (* bind the vertices buffer *)
  glBindBuffer GL_ARRAY_BUFFER mesh_buffers.(0);
  (* and link the buffer data with the shader program *)
  glVertexAttribPointerOfs32 vertexColorAttrib 3 VAttr.GL_FLOAT false 6 0;
  glVertexAttribPointerOfs32 vertexPositionAttrib 3 VAttr.GL_FLOAT false 6 3;

  (* active the indices buffer *)
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER mesh_buffers.(1);
  (* and render the mesh *)
  glDrawElements0 GL_TRIANGLES ndx_len Elem.GL_UNSIGNED_INT;

  (* desactivate the generic arrays *)
  glDisableVertexAttribArray vertexColorAttrib;
  glDisableVertexAttribArray vertexPositionAttrib;

  glUnuseProgram ();

  incr frame_count;
  glutSwapBuffers ();
;;



let vertex_shader = "
#version 130
in vec3 VertexColor;
in vec3 VertexPosition;
uniform mat4 ModelViewProjectionMatrix;
invariant gl_Position;
smooth out vec3 InterpolatedColor;

void main () {
    InterpolatedColor = VertexColor;
    gl_Position = ModelViewProjectionMatrix * vec4 (VertexPosition, 1.0);
}"

let fragment_shader = "
#version 130
precision highp float;
smooth in vec3 InterpolatedColor;
out vec4 Color;
void main() {
    Color = vec4 (InterpolatedColor, 1.0);
}"


let load_shaders vertexShader fragmentShader =
  let vertexShaderID = glCreateShader GL_VERTEX_SHADER in
  let fragmentShaderID = glCreateShader GL_FRAGMENT_SHADER in

  glShaderSource vertexShaderID vertexShader;
  glShaderSource fragmentShaderID fragmentShader;

  glCompileShader vertexShaderID;
  glCompileShader fragmentShaderID;

  if not(glGetShaderCompileStatus vertexShaderID) then begin
    prerr_endline "vertex shader compile error";
    prerr_endline (glGetShaderInfoLog vertexShaderID);
    glGetShaderCompileStatus_exn vertexShaderID;
  end;
  if not(glGetShaderCompileStatus fragmentShaderID) then begin
    prerr_endline "fragment shader compile error";
    prerr_endline (glGetShaderInfoLog fragmentShaderID);
    glGetShaderCompileStatus_exn fragmentShaderID;
  end;

  let shaderProgram = glCreateProgram () in
  glAttachShader shaderProgram vertexShaderID;
  glAttachShader shaderProgram fragmentShaderID;

  glLinkProgram shaderProgram;

  let uniformMatrix = glGetUniformLocation shaderProgram "ModelViewProjectionMatrix"
  and vertexPositionAttrib = glGetAttribLocation shaderProgram "VertexPosition"
  and vertexColorAttrib = glGetAttribLocation shaderProgram "VertexColor" in

  ( shaderProgram,
    uniformMatrix,
    vertexPositionAttrib,
    vertexColorAttrib,
    vertexShaderID,
    fragmentShaderID )



let make_mesh ~indices:ba_indices ~vertices:ba_vertices =
  let ndx_len = Bigarray.Array1.dim ba_indices in
  let shading = load_shaders vertex_shader fragment_shader in
  let mesh_buffers = glGenBuffers 2 in
  glBindBuffer GL_ARRAY_BUFFER mesh_buffers.(0);
  glBufferData GL_ARRAY_BUFFER (ba_sizeof ba_vertices) ba_vertices GL_STATIC_DRAW;
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER mesh_buffers.(1);
  glBufferData GL_ELEMENT_ARRAY_BUFFER (ba_sizeof ba_indices) ba_indices GL_STATIC_DRAW;
  (mesh_buffers, ndx_len, shading)


let delete_mesh
      (mesh_buffers, _,
       (shaderProgram,
        _, _, _,
        vertexShaderID,
        fragmentShaderID)) =

  glDeleteShader vertexShaderID;
  glDeleteShader fragmentShaderID;
  glDeleteProgram shaderProgram;
  glDeleteBuffers mesh_buffers;
;;


let keyboard mesh_with_shaders ~key ~x ~y =
  if key = '\027' then (delete_mesh mesh_with_shaders; exit 0);
;;


let last_time = ref(Unix.gettimeofday())

let rec timer ~value:msecs =
  glutTimerFunc ~msecs ~timer ~value:msecs;
  let now = Unix.gettimeofday() in
  let diff = (now -. !last_time) in
  Printf.printf " %d frames in %f seconds \t fps: %g\n%!"
                !frame_count diff (float !frame_count /. diff);
  frame_count := 0;
  last_time := now;
;;


let init_opengl ~width ~height =
  reshape ~width ~height;

  glEnable GL_DEPTH_TEST;
  glFrontFace GL_CCW;     (* assume a clean model *)
  glEnable GL_CULL_FACE;  (* activate elimination of polygons *)
  glCullFace GL_BACK;     (* remove back side of polygons *)
;;


(* main *)
let () =
  let width = 800 and height = 600 in
  ignore(glutInit Sys.argv);
  glutInitDisplayMode [GLUT_RGB; GLUT_DOUBLE; GLUT_DEPTH];
  glutInitWindowPosition ~x:100 ~y:100;
  glutInitWindowSize ~width ~height;
  ignore(glutCreateWindow ~title:"VBO with OpenGL 3.X");

  init_opengl ~width ~height;

  (* make a mesh ready to be drawn *)
  let mesh_with_shaders = make_mesh cube_indices cube_vertices in

  glutDisplayFunc ~display:(display mesh_with_shaders);
  glutKeyboardFunc ~keyboard:(keyboard mesh_with_shaders);
  glutIdleFunc ~idle:glutPostRedisplay;
  glutTimerFunc ~msecs ~timer ~value:msecs;
  glutReshapeFunc ~reshape;

  glutMainLoop ();
;;
open GL
open VertArray
open VBO
open Tsdl
open GLFW
open Glut
open Ogl_matrix

let width = 800
let height = 600

module Window =
struct

(* Using the OpenGL Core *)
let glfw_init ()= 
  init();
  windowHint ~hint:ContextVersionMajor ~value:3;
  windowHint ~hint:ContextVersionMinor ~value:3;
  windowHint ~hint:OpenGLProfile ~value:CoreProfile

(* Here we initialize the window *)
let glfw_instanciate_window ~height ~width ~title = 
  let window = GLFW.createWindow ~width:width ~height:height ~title:title () in 
  makeContextCurrent ~window:(Some window) ;

  let frame_buffer_size_callback = fun window width height -> glViewport ~x:0 ~y:0 ~width:width ~height:height in 
  setFramebufferSizeCallback ~window:window ~f:(Option.Some(frame_buffer_size_callback)) |> ignore ; 

  glViewport ~x:0 ~y:0 ~height:height ~width:width ; 
  window
end

type texture2D = {
  id : texture_id;
  size : int * int; (*  height * width *) 
  wrap_s : wrap_param;
  wrap_t : wrap_param;
  filter_Min : Min.min_filter; (*Filtering mode if texture pixels < screen pixels*)
  filter_Max : Mag.mag_filter; (*Filtering mode if texture pixels > screen pixels*)
  internal_format : InternalFormat.internal_format ;
  pixel_data_format : pixel_data_format ; 
}
module ResourceManager = 
struct
  let texturesBuffer = ref []
  let addTexture t = texturesBuffer := t::!texturesBuffer


  let generate_texture ((data : image_data), width, height, internal_format ,pixel_data_format) =
    let tex = 
    {
      id = glGenTexture ();
      size = (height,width);
      wrap_s = GL_REPEAT;
      wrap_t = GL_REPEAT;
      filter_Min = Min.GL_LINEAR;
      filter_Max = Mag.GL_LINEAR;
      internal_format = GL_RGB;
      pixel_data_format = GL_RGB;
    } in
    glBindTexture2D tex.id;
    
    (* Set Texture settings *)
    glTexParameter GL_TEXTURE_2D (TexParam.GL_TEXTURE_WRAP_S tex.wrap_s);
    glTexParameter GL_TEXTURE_2D (TexParam.GL_TEXTURE_WRAP_T tex.wrap_t);
    glTexParameter GL_TEXTURE_2D (TexParam.GL_TEXTURE_MIN_FILTER tex.filter_Min);
    glTexParameter GL_TEXTURE_2D (TexParam.GL_TEXTURE_MAG_FILTER tex.filter_Max);

    (* Loading texture in GPU buffer*)
    glTexImage2D GL_TEXTURE_2D 0 tex.internal_format width height tex.pixel_data_format GL_UNSIGNED_BYTE data;

    (* Unbind texture *)
    glUnbindTexture2D ();

    tex

  let bind_texture texture = 
    glBindTexture2D texture.id;;

  let load_texture_from_file path = 
    let format = Filename.extension path in 
    let s = 
    match format with 
    | ".jpeg" | ".jpg" -> 
      Jpeg_loader.load_img (Filename path) 
    | ".png" -> failwith "png is not supported yet"
    | _ -> failwith "unkmown format"
      in
    generate_texture s 
end

module Shader  =
struct
  let vertexShaderSource = 
  "#version 330 core
  layout (location = 0) in vec4 vertex; // <vec2 position, vec2 texCoords>

  out vec2 TexCoords;

  uniform mat4 model;
  uniform mat4 projection;

  void main()
  {
      TexCoords = vertex.zw;
      gl_Position = projection * model * vec4(vertex.xy, 0.0, 1.0);
  }";;
  let fragmentShaderSource = 
  "#version 330 core
  in vec2 TexCoords;
  out vec4 color;

  uniform sampler2D image;
  uniform vec3 spriteColor;

  void main()
  {    
      color =  vec4(spriteColor, 1.0) * texture(image, TexCoords); // multiply by ec4(spriteColor, 1.0) to add color 
  }";;
  let shaderP = ref None
  let getShaderProg () = Option.get (!shaderP)
  let init () = 
    let vertexShader = glCreateShader ~shader_type:GL_VERTEX_SHADER in
    glShaderSource ~shader:vertexShader vertexShaderSource;
    glCompileShader ~shader:vertexShader;
    if glGetShaderCompileStatus ~shader:vertexShader then () else print_endline "vertex shader compilation failed";
    print_endline (glGetShaderInfoLog ~shader:vertexShader);

    let fragmentShader = glCreateShader ~shader_type:GL_FRAGMENT_SHADER in
    glShaderSource ~shader:fragmentShader fragmentShaderSource;
    glCompileShader ~shader:fragmentShader;
    if glGetShaderCompileStatus ~shader:fragmentShader then () else print_endline "fragment shader compilation failed";

    (* shaderProgram, which is the linking of all shaders previously defined*)
    let shaderProgram = glCreateProgram() in
    glAttachShader ~program:shaderProgram ~shader:vertexShader;
    glAttachShader ~program:shaderProgram ~shader:fragmentShader;
    glLinkProgram ~program:shaderProgram;
    glUseProgram ~program:shaderProgram;
    (* deleting shaders, dont need anymore *)
    glDeleteShader~shader:vertexShader;
    glDeleteShader~shader:fragmentShader;
    glUseProgram shaderProgram;
    shaderP := Some shaderProgram

  let use () = 
    match !shaderP with
    |None -> failwith "No shader programm !";
    |Some _ -> glUseProgram (getShaderProg ())
  
  let setMatrix4 name mat = 
    let matrix = glGetUniformLocation (getShaderProg()) name in
    glUniformMatrix4fv matrix 1 false mat

  let setVector3 name vec = 
    let vector3 = glGetUniformLocation (getShaderProg()) name in
    glUniform3fv vector3 1 vec;
end

module SpriteRenderer =
struct
  

  let vao = ref 0
  let initRenderData () = 
    let verticies = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout [|
      (* position *)    (* texture coords *)
      0.0; 1.0;      0.0; 1.0;
      1.0; 0.0;     1.0;0.0;
      0.0; 0.0;    0.0;0.0;

      0.0; 1.0;      0.0; 1.0;
      1.0; 1.0;     1.0;1.0;
      1.0; 0.0;    1.0;0.0;
    |] in 
    let vbo = glGenBuffer () in
    vao := (glGenVertexArray());
    glBindVertexArray !vao;

    (* VBO *)
    glBindBuffer GL_ARRAY_BUFFER vbo;
    glBufferData GL_ARRAY_BUFFER (ba_sizeof verticies) verticies GL_STATIC_DRAW;

    
    let vertexPositionAttrib = glGetAttribLocation (Shader.getShaderProg ()) "vertex" in
    glEnableVertexAttribArray vertexPositionAttrib; (** layout (location = 0) in vec4 vertex *)
    glVertexAttribPointerOfs32 vertexPositionAttrib 4 GL_FLOAT  false 4 0;

    (* Unbinding *)
    glBindVertexArray 0
    (* glUnbindBuffer GL_ARRAY_BUFFER; *)

  let drawSprite ~texture2D ~position:((x,y) ) ~size:((s_x,s_y)) ~angle ~color =
    (* TODO : Wrap shader *)
    Shader.use ();

    let tmp = (Ogl_matrix.get_identity()) in
    Ogl_matrix.matrix_translate (x,y,0.0) tmp;

    (* Moving origin of location to center for rotation *)
    Ogl_matrix.matrix_translate (0.5*. s_x, 0.5 *. s_y,0.0) tmp;

    let angle_matrix = Ogl_matrix.z_rotation_matrix angle in 

    let tmp = Ogl_matrix.mult_matrix tmp angle_matrix in 
    (* Moving back the origin *)

    Ogl_matrix.matrix_translate (-0.5*. s_x, -0.5 *. s_y,0.0) tmp;

    let model = Ogl_matrix.mult_matrix tmp (Ogl_matrix.scale_matrix (s_x,s_y,1.0))  in (** Maybe the wrong order*)
    
    (* checkError(); *)
    Shader.setMatrix4 "model" model;
    Shader.setVector3 "spriteColor" color;

    glActiveTexture(GL_TEXTURE0);
    ResourceManager.bind_texture texture2D;

    glBindVertexArray !vao;
    
    glDrawArrays GL_TRIANGLES 0 6;
    glBindVertexArray 0;
end;;

module Camera = 
struct
  let projection = Ogl_matrix.ortho_projection 0.0 (float 800) (float 600) 0.0 (-1.0) 1.0
  let init() = 
    Shader.setMatrix4 "projection" projection

end

(* TESTING OPENGL, NOT FINAL CODE *)
  let render () = 

  (* For each gameObject do "that function" *)
    let texture = (List.hd !(ResourceManager.texturesBuffer)) in 
    let (w,h) = texture.size in
    SpriteRenderer.drawSprite  texture (300.0,200.0) (float h,float w) 45.0 [|1.0;1.0;1.0|]

let dt_ref = ref 0;;

let dt () = !dt_ref;;

let framerate = 1.;;

let processIntput window = 
  match getKey~window:window ~key:Escape with
  | true -> setWindowShouldClose ~window:window ~b:true
  |_ -> ();;

let rec gameLoop (last_time: float) (dt_cumulator:float) window: unit= 
  
  let t = Sys.time() in
  let dt = (t -. last_time) in
  processIntput window;

  (* Updating game state *)
  
  (* rendering *)
  glClearColor ~r:0.0 ~g:0.0 ~b:0.0 ~a:1.0;
  glClear ~mask: [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];
  glEnable (GL_DEPTH_TEST);
  render();


  swapBuffers window;
  pollEvents();


  if  not (windowShouldClose (window) )then
  
    gameLoop (t) (dt_cumulator +. dt) window
  
  else ()

  let test ()= 
    
    Window.glfw_init ();
    let window = Window.glfw_instanciate_window height width "The Game" in
    Shader.init();
    Shader.use ();
    Camera.init();
    ResourceManager.load_texture_from_file "res/megaman.jpg" |> ResourceManager.addTexture;
    SpriteRenderer.initRenderData();
    glEnable (GL_DEPTH_TEST);

    (* GAME LOOP *)
    glPolygonMode GL_FRONT_AND_BACK GL_FILL ;
    gameLoop 0. 0. window;
    


    terminate();;


    


test ();;
  
open GL
open VertArray
open VBO
open Tsdl
open GLFW
open Glut
open Ogl_matrix

let width = 800
let height = 600
let dt_ref = ref 0.0;;

let dt () = !dt_ref;;

let framerate = 1.;;
let checkError () = let e = (glGetError()) in
match e with
| GL.GL_NO_ERROR -> print_string "NO ERROR"
| GL.GL_INVALID_ENUM -> failwith "1"
| GL.GL_INVALID_VALUE -> failwith "2"
| GL.GL_INVALID_OPERATION -> failwith "3"
| GL.GL_STACK_OVERFLOW -> failwith "4"
| GL.GL_STACK_UNDERFLOW -> failwith "5"
| GL.GL_OUT_OF_MEMORY -> failwith "6"
| GL.GL_TABLE_TOO_LARGE -> failwith "7";;

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
  (* let texturesBuffer = ref []
  let addTexture t = texturesBuffer := t::!texturesBuffer *)


  let generate_texture ((data : image_data), width, height, internal_format ,pixel_data_format) =
    let tex = 
    {
      id = glGenTexture ();
      size = (height,width);
      wrap_s = GL_REPEAT;
      wrap_t = GL_REPEAT;
      filter_Min = Min.GL_NEAREST;
      filter_Max = Mag.GL_NEAREST;
      internal_format = internal_format;
      pixel_data_format = pixel_data_format;
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
    | ".png" -> Png_loader.load_img(Filename path)
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
  uniform vec4 spriteColor;

  void main()
  {    
      color = spriteColor * texture(image, TexCoords); // multiply by ec4(spriteColor, 1.0) to add color 
      //color = vec4(color.xyz,0.5);
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
    glUniform3fv vector3 1 vec
  let setVector4 name vec = 
    let vector4 = glGetUniformLocation (getShaderProg()) name in
    glUniform4fv vector4 1 vec;
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
    glBindVertexArray 0;
    glUnbindBuffer GL_ARRAY_BUFFER

  let drawSprite ~texture2D ~position:((x,y,z) ) ~size:((s_x,s_y)) ~angle ~color =
    (* TODO : Wrap shader *)
    Shader.use ();

    let tmp = (Ogl_matrix.get_identity()) in
    Ogl_matrix.matrix_translate (x,y,z) tmp;

    (* Moving origin of location to center for rotation *)
    Ogl_matrix.matrix_translate (0.5*. s_x, 0.5 *. s_y,0.0) tmp;

    let angle_matrix = Ogl_matrix.z_rotation_matrix angle in 

    let tmp = Ogl_matrix.mult_matrix tmp angle_matrix in 
    (* Moving back the origin *)

    Ogl_matrix.matrix_translate (-0.5*. s_x, -0.5 *. s_y,0.0) tmp;

    let model = Ogl_matrix.mult_matrix tmp (Ogl_matrix.scale_matrix (s_x,s_y,1.0))  in (** Maybe the wrong order*)
    
    (* checkError(); *)
    Shader.setMatrix4 "model" model;
    Shader.setVector4 "spriteColor" color;

    glActiveTexture(GL_TEXTURE0);
    (* glActiveTexture(GL_TEXTURE2); *)
    ResourceManager.bind_texture texture2D;

    glBindVertexArray !vao;
    
    glDrawArrays GL_TRIANGLES 0 6;
    glBindVertexArray 0;
end;;

module Camera = 
struct
  let projection = Ogl_matrix.ortho_projection 0.0 (float width) (float height) 0.0 (-1.0) 1.0
  let init() = 
    Shader.setMatrix4 "projection" projection

end

type transform = 
{
  position : float*float*float;
  scale : float*float;
  rotation : float;
};;


class animation (textures) (loop)= 
object(self)
  val textures : texture2D list = textures
  val mutable texturesFlow : texture2D list = []
  val mutable speed : float = 1.0
  val mutable counter = 0.0
  val color = [|1.0;1.0;1.0;1.0|] 
  val loop = loop
  
  method get_color () = color
  method set_speed s = speed<- s
  method drawCurrentFrame (transform) = 
  counter <- (counter +. dt());
  let textureMatch  = 
    if (counter < speed) then begin
    List.hd texturesFlow 
    end
    else
    begin
    counter <- 0.0;
    match texturesFlow with
      |[] -> failwith "Animation : No datas !"
      |a::[] -> if loop then begin texturesFlow <- textures end;a
      |h::t -> texturesFlow <- t; h 
    end 
      in
  let s = 
  (* let (h,w) = textureMatch.size in 
  print_float counter;
  print_newline(); *)
    let ((x,y),(sx,sy))  = (textureMatch.size, transform.scale) in 
    ((float x)*.sx,(float y)*.sy) in
  SpriteRenderer.drawSprite 
  textureMatch
  transform.position
  s
  transform.rotation
  color;
  
  (* c'est dla merde lol *)
  initializer texturesFlow <- textures
end
class spriteRenderer (animations) = 
object(self)
  val animations : (string * animation) list = animations
  val mutable currentAnimation : animation = (new animation [] true)
  method set_animation name = currentAnimation <- (List.assoc name) animations
  method draw transform = 
  (* print_float (dt()); *) 
    (currentAnimation)#drawCurrentFrame transform
  
  initializer begin let _,a = List.hd animations in currentAnimation <- a end

end
(* Load images *)


(* TESTING OPENGL, NOT FINAL CODE *)
  let renderObject renders = 
    glClearColor ~r:0.5 ~g:0.5 ~b:0.5 ~a:1.0;
    glClear ~mask: [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];
    glEnable (GL_DEPTH_TEST);
    let trans = {
      position = 10.0, 10.0, 0.0;
      scale = 5.0,5.0;
      rotation = 10.0;

         } in
    List.iter (fun e -> e#draw trans) renders
  (* For each gameObject do "that function" *)
  (* render1#update();; *)
    (* List.iteri (fun i texture ->

      let (w,h) = texture.size in
      SpriteRenderer.drawSprite  texture (300.0,200.0,1.0*.(float i)) (500.0,500.0) (float (i+1)  *. 45.0) [|1.0;1.0;1.0;1.0|]
    
    )[ResourceManager.load_texture_from_file "res/chara/Sprite-0001.jpg";ResourceManager.load_texture_from_file "res/chara/Sprite-0002.jpg"]
 *)


let processIntput window = 
  match getKey~window:window ~key:Escape with
  | true -> setWindowShouldClose ~window:window ~b:true
  |_ -> ();;

let rec gameLoop (last_time: float) (dt_cumulator:float) window render : unit= 
  let t = Unix.gettimeofday() in
  dt_ref := (t -. last_time);
  


  processIntput window;

  (* Updating game state *)
  
  (* rendering *)
  renderObject [render];
  (* checkError(); *)

  swapBuffers window;
  pollEvents();


  if  not (windowShouldClose (window) )then
  begin
    (* print_float (Unix.gettimeofday());
    print_newline(); *)
    gameLoop ( t) (dt_cumulator +. dt()) window render
  end
  else ()

  let test ()= 
  
    Window.glfw_init ();
    let window = Window.glfw_instanciate_window height width "The Game" in
    Shader.init();
    Camera.init();
    SpriteRenderer.initRenderData();
    glEnable GL_BLEND;
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA;  
    glEnable (GL_DEPTH_TEST);

    let textures = 
    let e1 = ResourceManager.load_texture_from_file "res/chara/Sprite-0003.jpg"  in 
    let e2 = ResourceManager.load_texture_from_file "res/chara/Sprite-0002.jpg"  in 
    let e3 = ResourceManager.load_texture_from_file "res/chara/Sprite-0003.jpg"  in 
    let e4 = ResourceManager.load_texture_from_file "res/chara/Sprite-0004.jpg"  in 
    [e1;e2;e3;e4]in
    let anim1 = new animation textures true in
    let render1 = new spriteRenderer [("idle",anim1)] in
    
    (* ResourceManager.load_texture_from_file "res/megaman.jpg" |> ResourceManager.addTexture;
    ResourceManager.load_texture_from_file "res/container.jpg" |> ResourceManager.addTexture; *)

    (* GAME LOOP *)
    glPolygonMode GL_FRONT_AND_BACK GL_FILL ;
    gameLoop 0. 0. window render1;
    


    terminate();;


    


test ();;

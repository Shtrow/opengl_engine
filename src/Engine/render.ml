open GL
open VertArray
open VBO
open GLFW
open Math
type spriteCoord = 
{
  position : float*float*float;
  scale : float*float;
  rotation : float;
}
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
type window = GLFW.window
let textures : (string* texture2D) list ref= ref []

let textures_data :  ('string *
 (GL.image_data * int * int * GL.InternalFormat.internal_format *
  GL.pixel_data_format))
list ref = ref []

let width = 800
let height = 600

let ref_dt = ref 0.0
let dt () = !ref_dt

let to_sprite_coord (t:Math.Transform.transform) =
  let (x, y) = t.position in
  {
    position = (x,y,t.depth);
    scale = t.scale;
    rotation = t.angle;
  }

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

  let frame_buffer_size_callback =
    fun window width height -> glViewport ~x:0 ~y:0 ~width:width ~height:height in 
  setFramebufferSizeCallback ~window:window 
    ~f:(Option.Some(frame_buffer_size_callback)) |> ignore; 

  glViewport ~x:0 ~y:0 ~height:height ~width:width; 
  window
end


module ResourceManager = 
struct
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
    glTexImage2D GL_TEXTURE_2D 0 tex.internal_format width height 
      tex.pixel_data_format GL_UNSIGNED_BYTE data;

    (* Unbind texture *)
    glUnbindTexture2D ();

    tex

  let bind_texture texture = 
    glBindTexture2D texture.id

  let load_texture_from_file ~name ~path = 
    let format = Filename.extension path in 
    let s = 
    match format with 
    | ".jpeg" | ".jpg" -> 
      Jpeg_loader.load_img (Filename path) 
    | ".png" -> Png_loader.load_img(Filename path)
    | _ -> failwith "unkmown format"
      in
      textures_data := (name,s)::(!textures_data)
  
  let load_textures () = 
    let t =  
  ( List.map (fun (name,e) ->
        (name,(generate_texture e))
      ) !textures_data)
    in
    textures := t

  let get_texture name = 
  
    if List.exists (fun (e,t) -> String.equal e name) (!textures) then 
    List.assoc name !textures
    else failwith ""

end

module Shader  =
struct
  let vertexShaderSource = 
  "#version 330 core
  layout (location = 0) in vec4 vertex;

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
      color = spriteColor * texture(image, TexCoords);
  }";;
  let shaderP = ref None
  let getShaderProg () = Option.get (!shaderP)
  let init () = 
    let vertexShader = glCreateShader ~shader_type:GL_VERTEX_SHADER in
    glShaderSource ~shader:vertexShader vertexShaderSource;
    glCompileShader ~shader:vertexShader;
    if glGetShaderCompileStatus ~shader:vertexShader then () 
    else print_endline "vertex shader compilation failed";

    print_endline (glGetShaderInfoLog ~shader:vertexShader);

    let fragmentShader = glCreateShader ~shader_type:GL_FRAGMENT_SHADER in
    glShaderSource ~shader:fragmentShader fragmentShaderSource;
    glCompileShader ~shader:fragmentShader;
    if glGetShaderCompileStatus ~shader:fragmentShader then ()
      else print_endline "fragment shader compilation failed";

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
      1.0; 0.0;      1.0; 0.0;
      0.0; 0.0;      0.0; 0.0;

      0.0; 1.0;      0.0; 1.0;
      1.0; 1.0;      1.0; 1.0;
      1.0; 0.0;      1.0; 0.0;
    |] in 
    let vbo = glGenBuffer () in
    vao := (glGenVertexArray());
    glBindVertexArray !vao;

    (* VBO *)
    glBindBuffer GL_ARRAY_BUFFER vbo;
    glBufferData GL_ARRAY_BUFFER (ba_sizeof verticies) verticies GL_STATIC_DRAW;

    
    let vertexPositionAttrib = glGetAttribLocation (Shader.getShaderProg ()) "vertex" in
    glEnableVertexAttribArray vertexPositionAttrib; (* layout (location = 0) in vec4 vertex *)
    glVertexAttribPointerOfs32 vertexPositionAttrib 4 GL_FLOAT  false 4 0;

    (* Unbinding *)
    glBindVertexArray 0;
    glUnbindBuffer GL_ARRAY_BUFFER

  let drawSprite ~texture2D ~position:((x,y,z) ) ~size:((s_x,s_y)) ~angle ~color =
    Shader.use ();

    let tmp = (Ogl_matrix.get_identity()) in
    Ogl_matrix.matrix_translate (x,y,z) tmp;

    (* Moving origin of location to center for rotation *)
    Ogl_matrix.matrix_translate (0.5*. s_x, 0.5 *. s_y,0.0) tmp;

    let angle_matrix = Ogl_matrix.z_rotation_matrix angle in 

    let tmp = Ogl_matrix.mult_matrix tmp angle_matrix in 
    (* Moving back the origin *)

    Ogl_matrix.matrix_translate (-0.5*. s_x, -0.5 *. s_y,0.0) tmp;

    let model = Ogl_matrix.mult_matrix tmp (Ogl_matrix.scale_matrix (s_x,s_y,1.0))  in
    
    (* checkError(); *)
    Shader.setMatrix4 "model" model;
    Shader.setVector4 "spriteColor" color;

    glActiveTexture(GL_TEXTURE0);
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
  let zoom off = 
    let p = projection in 
    Ogl_matrix.matrix_translate (0.,0.,off) p;
    Shader.setMatrix4 "projection" p

  let move (x,y) = 
    let p = projection in
    Ogl_matrix.matrix_translate (x,y,0.) p;
    Shader.setMatrix4 "projection" p
  

end



class animation (textures) (loop)= 
object(self)
  val textures : texture2D list = textures
  val mutable texturesFlow : texture2D list = []
  val mutable speed : float = 0.5
  val mutable counter = 0.0
  val color = [|1.0;1.0;1.0;1.0|] 
  val loop = loop
  method is_finished ()  = List.length texturesFlow = 1
  method rewind () = texturesFlow <- textures
  method get_color () = color
  method set_speed s = speed<- s
  method drawCurrentFrame (spriteCoord) = 
  counter <- (counter +. dt());
  let textureMatch  = 
    if (counter < speed) then List.hd texturesFlow 
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
    let ((x,y),(sx,sy))  = (textureMatch.size, spriteCoord.scale) in 
    ((float y)*.sy,(float x)*.sx) in
    SpriteRenderer.drawSprite 
    textureMatch
    spriteCoord.position
    s
    spriteCoord.rotation
    color;
  initializer texturesFlow <- textures
end
class animRenderer (animations) = 
object(self)
  val animations : (string * animation) list = animations
  val mutable currentAnimation : animation = (new animation [] true)
  method get_current_anim = currentAnimation
  method set_animation name = currentAnimation <- (List.assoc name) animations
  method get_animation name = (List.assoc name) animations
  method draw spriteCoord = 
    (currentAnimation)#drawCurrentFrame spriteCoord
  initializer begin let _,a = List.hd animations in currentAnimation <- a 
end

end
let init_graphic () = 

    Window.glfw_init ();
    let window = Window.glfw_instanciate_window height width "The Game" in
    Shader.init();
    Camera.init();
    SpriteRenderer.initRenderData();
    glEnable GL_BLEND;
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA;  
    glEnable (GL_DEPTH_TEST);
    window

let update_graphic window = 
  (* let font = Ftgl.ftglCreatePixmapFont "/usr/share/fonts/TTF/Hack-Bold.ttf" in
  
  Ftgl.ftglSetFontFaceSize font 72 72;
  Ftgl.ftglRenderFont font "Hello World!" FTGL_RENDER_ALL; *)

    swapBuffers window;
    pollEvents()
let clear () = 
  glClearColor ~r:0.0 ~g:0.0 ~b:0.0 ~a:0.0;
  glClear ~mask: [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];
  glEnable (GL_DEPTH_TEST)

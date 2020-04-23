type vector = float * float

let up = (0.0,1.0)
let down = (0.0,-1.0)
let left = (-1.0,0.0)
let right = (1.0,0.0)

let rad_to_degree angle = angle *. (180.0 /. Float.pi)
let degree_to_rad angle = angle *. ( Float.pi /.180.0)

let lenght (v_x,v_y) = 
  (v_x *.v_x)+.(v_y*.v_y) |> sqrt

let distance (v1_x,v1_y) (v2_x,v2_y) = 
((v2_x-. v1_x)*.(v2_x-. v1_x)) +.   ((v2_y-. v1_y)*.(v2_y-. v1_y)) |> sqrt

let normalize ((v_x,v_y) as v) = 
  let l = lenght v in 
  v_x /. l , v_y /. l

let (-) (u1,u2) (v1,v2) = 
u1-.v1,u2-.v2

let (+) (u1,u2) (v1,v2) = 
u1+.v1,u2+.v2

let mul_scalar scalar (u1,u2)  = 
u1*.scalar,u2*.scalar

let mul (u1,u2) (v1,v2) = 
  u1*.v1,u2*.v2

let degree_to_vector angle= 
let angle = rad_to_degree angle in
cos angle, sin angle

let vecI (x, y)= (Float.to_int x,Float.to_int y) 

let vecF (x, y)= (float x,float y) 

let vector_to_degree (x,y) = 
  Float.atan2 y x
 |> rad_to_degree

type transform = 
{
  position : float*float;
  scale : float*float;
  angle : float; (* In degree *)
  depth : float; (* correspond to the third coordonate of the position vector *)
  pivot : float *float
}

let distance src dst = 
  let (src_x,src_y) = src.position in  
  let (dst_x,dst_y) = dst.position in  
  ((dst_x-. src_x)*.(dst_x-. src_x)) +.   ((dst_y-. src_y)*.(dst_y-. src_y)) |> sqrt


let lookAt src dst = 
  let p_src = src.position in  
  let p_dst = dst.position in
  let new_angle =
  Vector2.normalize (Vector2.(-) p_src p_dst) |> Vector2.vector_to_degree in
  {src with angle = new_angle +. 90.0}

(* Direction should be multiply by dt *)
let translate t direction = 
  let (+) = Vector2.(+) in 
  {t with position = (t.position + direction) }


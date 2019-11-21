let i_bytes = Marshal.to_bytes 5 [Marshal.Compat_32];;

class a_class = 
object
method g = "i'm a A instance"
end;;

(* subtype of a_class *)
let b_object = 
object
inherit a_class
method g = "i'm a B instance"
end;;

let pp_bytes = Marshal.to_bytes b_object [Marshal.Closures];;

let get_component b = 

	Marshal.from_bytes b 0;;

let i_val : int = get_component i_bytes;;

let pp_val : a_class = get_component pp_bytes;;

print_int i_val;;

print_newline();;

print_endline (pp_val#g);;
let i_bytes = Marshal.to_bytes 5 [Marshal.Compat_32];;

class a_class = 
object
method g = 1
end;;


class b_class = 
    object
    inherit a_class
    method g = 2
    method h =  print_string "supplementary method"
    end;;

class c_class = 
    object
    inherit a_class
    method g = 3
    method h = print_string "supplementary method"
    method f = print_string "Third method"
    end;;


let a = new a_class;;
let b = new b_class;;
let c = new c_class;;

let nos = [a;b;c];;  (* Impossible !*)

let bytes_constr x = Marshal.to_bytes x [Marshal.Closures];;

let get_coponent c n = let comp = Marshal.from_bytes c 0 in 
    if comp#g = n then comp else None;;

let nos = [bytes_constr a; bytes_constr b; bytes_constr c];;

let rec take n l = match l with 
    []-> failwith "empty"
    | h::t -> let comp = (get_coponent h n) in if comp <> None then Option.get comp
                                                else take n t;;

let d : c_class = take 3 nos;;

(* En théorie c'est bon, mais en pratique il y a seg fault *)
d#f;;






(* subtype of a_class *)
let b_object = 
object
inherit a_class
method g = 2
method h =  print_string "supplementary method"
end;;

let c_object = 
object
inherit a_class
method g = 3
method h = print_string "supplementary method"
method f = print_string "Third method"
end;;

let list = [b_object; c_object];;

let listb = [pb_bytes; pc_bytes];;

let pb_bytes = Marshal.to_bytes b_object [Marshal.Closures];;
let pc_bytes = Marshal.to_bytes c_object [Marshal.Closures];;


let get_component i l = match l with 
    []-> None
    | [a]-> 

let get_component b = 

	Marshal.from_bytes b 0;;

let i_val : int = get_component i_bytes;;

let pp_val : a_class = get_component pb_bytes;;

print_int i_val;;

print_newline();;

print_endline (pp_val#g);;
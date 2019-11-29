class me =
  object
    method nom = "Henri"
    method age = 23
  end;;

let henri = new me;;

let hhenri = henri, `Me henri;;

exception Typecase;;
let is_me = function _, `Me q -> true | _ -> false;;
let to_me = function _, `Me q -> q | _ -> raise Typecase;;
let as_me = function pq -> (pq :> me * _);;

class me2 =
  object
    inherit me
    method nom = "Benjamin"
    method age = 23
    method sexe = 'M'
  end;;


let benjamin = new me2;;

let ben = (benjamin :> me), `Me henri;;

let is_me2 = function _, `Me2 q -> true | _ -> false;;
let to_me2 = function _, `Me2 q -> q | _ -> raise Typecase;;
let as_me2 = function pq -> (pq :> me2 * _);;

let pissa = hhenri :: ben :: [];;

let g = List.nth pissa 1;;


	
let l =
  let ( @:: ) x y = (as_me x) :: y in
    hhenri @:: ben @:: [];;

let nom p = (fst p)#nom;;
List.map nom l;;

	
let age p =  if is_me2 p then (to_me2 p) # age else 0;;
List.map age l;;




(* Création d'une classe virtuelle mère*)
class virtual ['a] a_class = 
object
method virtual soi : 'a
end;;

(* une classe b qui hérite de l'interface a*)
class ['a] b_class = 
object (self)
inherit ['a] a_class
method soi = `ClassB self (* pointeur vers l'objet*)
method h =  print_string "supplementary method\n"
end;;

(* une classe c qui hérite de l'interface a*)
class ['a] c_class = 
object (self)
inherit ['a] a_class (**)
method soi = `ClassC self
method g = 3
method h = print_string "supplementary method\n"
method f = print_string "Third method\n"
end;;

(*Deux nouvelle instances*)
let b = (new b_class:> _ a_class);;
let c = (new c_class:> _ a_class);;

(* un tableau de _ a_class*)
let l =[b;c];;

(* Une fonction qui permet d'accéder à la méhode de son choix*)

let geth = function
`ClassB b -> b#h
| `ClassC c -> c#h;;

let getf = function
`ClassB b -> failwith "Il n'y a pas de méthode f\n"
| `ClassC c -> c#f;;

let c = (new c_class :> _ a_class);;
getf c#soi;;
let b = (new b_class :> _ a_class);;


List.iter (fun o -> geth o#soi) l;;

getf b#soi;;

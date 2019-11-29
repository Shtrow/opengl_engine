
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
method g = 2
method h =  print_string "supplementary method"
end;;

(* une classe c qui hérite de l'interface a*)
class ['a] c_class = 
object (self)
inherit ['a] a_class (**)
method soi = `ClassC self
method g = 3
method h = print_string "supplementary method"
method f = print_string "Third method"
end;;

(*Deux nouvelle instances*)
let b = (new b_class:> _ a_class);;
let c = (new c_class:> _ a_class);;

(* un tableau de _ a_class*)
let l =[b;c];;

(* Une fonction qui permet d'accéder à la méhode de son choix*)
let soi o n = match o with
`ClassB b -> if n = 1 then b#g else b#h
| `ClassC c -> if n = 1 then c#g else if n=2 then c#h else c#f;;

type  ground = Grass | Wall | Nothing | Void | Glass


let char_to_tile = function
  |'w' -> Wall
  |'.'-> Void
  |'g' -> Glass
  | _ -> Nothing

let parse path = 
  let ic = open_in path in 
  let stream = Stream.from (
      fun _ -> 
        try Some (input_line ic ) with End_of_file -> None
   ) in
  let rec build_map buff =
    try let line = Stream.next stream  in 
    print_int @@ String.length line;
    print_endline line;
     build_map (List.of_seq ( Seq.map (char_to_tile) (String.to_seq line)) :: buff)
    with Stream.Failure ->
      buff
    in
  build_map [[]]
  
(*
let test1 = 
  let b = "./map1" in 
  let b = parse b in
  List.iter(

    print_newline();
    List.iter (
      print_newline ();
        function 
          |Wall -> print_char 'w'
          | Void -> print_char '.'
          | _ -> print_char 'T'
    )
  ) b
  *)

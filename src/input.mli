
type key_code = 
NoOne | UP |  DOWN | LEFT | RIGHT | A| B | C | D | E | F | G | H | I | J  | K | L | M | N | O | P| Q| R| S| T | U| V| W| X| Y | Z

(* True while the key is pressed *)
val keyIsDown : key_code -> bool

(* True during the frame when the key is pressed *)
val keyIsPressed : key_code -> bool

(* True during the frame when the key is released *)
val keyIsReleased : key_code -> bool


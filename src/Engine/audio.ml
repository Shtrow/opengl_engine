open SFSound
open SFSoundBuffer

type sound = SFSound.t

let load_sound path = 
  let buffer = loadFromFile path in 
  let sound = create () in 
  setBuffer sound buffer;
  sound

let play sound = 
  play sound
  
let set_volume sound v  =
  setVolume sound v



type entity;;
class type component  = 
  object
    val mutable entity : entity ref
  end
class type entity  = 
  object
    val mutable components : component list

    method update : unit -> unit
  end

class type action = 
  object 
    method perform : unit -> unit
  end 


class type actor = 
  object
    inherit entity
    val mutable cooldown : int
    val mutable actions : action list
    val mutable is_ready : bool
    method take_action : unit -> unit 
  end
type rectangle = {height : int ; width : int}
type position = {x : int ; y : int}
type scale = {factor_height : int ; factor_width : int}


class type collision_box = 
  object 
    val box : rectangle
  end

class type transform = 
  object 
    inherit component
  end

class type scene = 
  object
    val mutable entities : entity list
    method gameUpdate : unit -> unit
  end
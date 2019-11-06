
class  type ['component]  entity  = 
  object
    val mutable components : 'component list
    method get_components : 'component list
  end
class type virtual component  = 
  object
    val mutable _entity : component entity ref
    method virtual update : unit 
  end

class type virtual action = 
  object 
    method virtual perform : unit
  end 


class type virtual actor = 
  object
    inherit [component] entity
    val mutable cooldown : int
    val mutable actions : action list
    val mutable is_ready : bool
    method virtual take_action : unit
  end
type rectangle = {height : int ; width : int}
type position = {x : int ; y : int}
type scale = {factor_height : int ; factor_width : int}


class type collision_box = 
  object 
    val box : rectangle
  end

class type transform  = 
  object 
    inherit component
    method update : unit
  end

class type scene = 
  object
    val mutable entities : (component entity) list
    method gameUpdate : unit
  end

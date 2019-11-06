type rectangle = {height : int ; width : int}
type position = {x : int ; y : int}
type scale = {factor_height : int ; factor_width : int}

class entity  (components : component list)= 
  object(self)
    val mutable components : component list = components
    method add_component c = 
      components <- c::components
    method get_components = ref components
  end

and virtual component (entity :  entity ref)= 
  object(self)
    val mutable _entity : entity ref = entity 
    method get_entity_ref = _entity
    method update : unit = ()
  end

class virtual action = 
  object(self)
    method virtual perform : unit 
  end

class virtual actor (components : component list) (cd : int) (acts : action list)= 
  object(self)
    inherit entity components
    val mutable cooldown = cd
    val mutable actions =acts
    val mutable is_ready  = false
    method virtual take_action : unit  
  end

class collision_box (box : rectangle)= 
  object(self)
    val mutable box : rectangle = box
  end

class transform (entity : entity ref)= 
  object(self)
    inherit component entity
    method update = ()
  end

class scene (entities : entity list)  = 
  object(self)
    val mutable entities  = entities
    method gameUpdate = 
    let iter_on_component = 
      List.iter (fun c ->c#update) in
    List.iter ( fun e -> iter_on_component !(e#get_components)) entities
  end

class virtual actor cd acts = 
  object (self)
    inherit entity
    val mutable cooldown = cd
    val mutable actions = []
    val mutable is_ready = false
    method virtual take_action = unit -> unit
  end
  
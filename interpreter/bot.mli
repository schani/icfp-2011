open Cards

val botloop: ?skipfirst:bool -> (default_world_type intercontext -> default_world_type -> turn option -> a' -> turn * a') -> a' -> unit

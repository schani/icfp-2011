open Inter
open Parser

let default_world_printer world = ()

let _ = 
  play_game default_context (create_default_world ()) parse_input parse_input default_world_printer

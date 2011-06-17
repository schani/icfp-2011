open Inter
open Parser

let _ = 
  play_game default_context (create_default_world ()) parse_input parse_input

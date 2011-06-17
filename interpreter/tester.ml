open Inter
open Parser
open Printer

let default_world_printer world = ()

let _ = 
  play_game default_context (create_default_world ()) parse_input parse_input std_world_printer

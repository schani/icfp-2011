open Inter
open Parser
open Printer

let default_world_printer world = ()

let _ =
  play_game default_context (create_default_world ()) (parse_input stdin) (parse_input stdin) std_world_printer

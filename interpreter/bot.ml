open Inter
open Parser
open Printer
open Arg
open Cards

let bootloop ?(skipfirst=false) move_callback priv_data =
  let debug = true
  in let debug_context_error default_context_error = 
      fun string world -> (
	Printf.printf "Exception: %s\n" string;
	default_context_error string world
      )
  in let context = if debug then 
      {default_context with error = (debug_context_error default_context.error)} 
    else 
      default_context
     and printer = quiet_printer
  in
  let rec loop ?(skipfirst=false) proponent_move world priv_data =
    let world, priv_data  =
      if not skipfirst
      then
	let move, priv_data = move_callback context world proponent_move priv_data
	in
	  print_string (msg_of_turn move);
	  flush stdout;
	  let _, world, _ = apply_player context world move printer
	  in
	    world, priv_data
      else
	world, priv_data
    in let move2 = (parse_input stdin printer ())
    in let _, world, _ = apply_player context world move2 printer
    in
      loop proponent_move world priv_data
  in
    loop ~skipfirst None (create_default_world ()) priv_data

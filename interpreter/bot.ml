open Inter
open Parser
open Printer
open Arg
open Cards

let read_turns_from_file filename =
  let ifi = open_in filename
  in let l = ref []
  in
    try
      while true
      do
	l := (parse_input ifi  quiet_printer ()) :: !l
      done;
      []
    with
	End_of_file -> List.rev !l

let bootloop move_callback priv_data =
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
      loop (Some move2) world priv_data
  in let skipfirst = match Sys.argv with
    | [| _; "0" |] -> false
    | [| _; "1" |] -> true
    | _ -> failwith (Printf.sprintf "%s: usage: %s 0|1" Sys.argv.(0) Sys.argv.(0))
  in
    loop ~skipfirst None (create_default_world ()) priv_data

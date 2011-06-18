open Inter
open Parser
open Printer
open Arg
open Cards

let debug = true
let debug_context_error default_context_error = 
  fun string world -> (
    Printf.printf "Exception: %s\n" string;
    default_context_error string world
  )

let read_turns_from_file ifi printer =
  let l = ref []
  in
    try
      while true
      do
	l := (parse_input ifi printer ()) :: !l
      done;
      !l
    with
	End_of_file -> List.rev !l

let rec run_bot context world printer turns =
  let move, storedturns = match turns with
    | turn :: rest -> turn, rest
    | _ -> Left (I, 0), []
  in
    print_string (string_of_turn move);
    flush stdout;
    let count, world, _ = apply_player context world move printer
    in let move2 = (parse_input stdin printer ())
    in
      let count,world,_ = apply_player context world move2 printer
      in
	run_bot context world printer storedturns

let _ =
  let ifi = open_in "/icfpnfs/SCHANI/killer-fn.cmd"
  and printer = quiet_printer
  and botmode = match Sys.argv with
    | [| _; "0" |] -> 0
    | [| _; "1" |] -> failwith "please start me with 0"
    | _ -> failwith "dunno what to do, gimme some args"
  in let turns = read_turns_from_file ifi printer 
  in let context = if debug then 
      {default_context with error = (debug_context_error default_context.error)} 
    else 
      default_context 
  in
    run_bot context (create_default_world ()) printer turns

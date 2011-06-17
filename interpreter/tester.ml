open Inter
open Parser
open Printer
open Arg

type running_mode =
  | RM_UNKNOWN
  | RM_ONLY
  | RM_ALT
  | RM_MATCH of (string * string)

let runmode = ref RM_UNKNOWN

let default_world_printer world = ()

let output_callback handle turn =
  match handle with
    | None -> ()
    | Some handle -> output_string handle (msg_of_turn turn)

let _ =
  let parse_otherargs str =
    runmode :=
      match (Str.split (Str.regexp " +") str) with
	| ["only"] -> RM_ONLY
	| ["alt"] -> RM_ALT
	| ["match"; p1; p2] -> RM_MATCH (p1, p2)
	| _ -> failwith "illegal argument"
  in
    Arg.parse [] parse_otherargs "only | alt | match <prog1> <prog2>";
    match !runmode with
      | RM_UNKNOWN -> failwith "dunno what to do, gimme some args"
      | RM_ONLY -> failwith "not yet implemented"
      | RM_ALT ->
	  play_game default_context (create_default_world ())
	    (parse_input stdin) (output_callback None)
	    (parse_input stdin) (output_callback None)
	    std_world_printer
      | RM_MATCH (p1, p2) ->
	  let p1_in_channel, p1_out_channel = Unix.open_process ("p1" ^ " 0")
	  and p2_in_channel, p2_out_channel = Unix.open_process ("p1" ^ " 1")
	  in
	    play_game default_context (create_default_world ())
	      (parse_input p1_in_channel) (output_callback (Some p1_out_channel))
	      (parse_input p2_in_channel) (output_callback (Some p2_out_channel))
	      std_world_printer

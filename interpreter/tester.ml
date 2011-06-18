open Inter
open Parser
open Printer
open Arg
open Cards

type running_mode =
  | RM_UNKNOWN
  | RM_ONLY
  | RM_ALT
  | RM_MATCH of (string * string)

let runmode = ref RM_UNKNOWN

let debug_context_error default_context_error = 
  fun string world -> (
    Printf.printf "Exception: %s\n" string;
    default_context_error string world
  )

let debug = true

let default_world_printer world = ()

let output_callback handle turn =
  match handle with
    | None -> ()
    | Some handle ->
	output_string handle (msg_of_turn turn);
	flush handle

let _ =
  let compute_runmode () =
    match Sys.argv with
      | [|_; "only"|] -> RM_ONLY
      | [|_; "alt"|] -> RM_ALT
      | [|_; "match"; p1; p2|] -> RM_MATCH (p1, p2)
      | _ -> failwith "illegal arguments"
  in
  let context = if debug then 
{default_context with error = (debug_context_error default_context.error)} 
    else 
      default_context 
  in
    match compute_runmode () with
      | RM_UNKNOWN -> failwith "dunno what to do, gimme some args"
      | RM_ONLY -> 
	  play_game context (create_default_world ())
	    (parse_input stdin) (output_callback None)
	    (fun () -> Left(I,0)) (output_callback None)
	    std_world_printer 0
      | RM_ALT ->
	  play_game context (create_default_world ())
	    (parse_input stdin) (output_callback None)
	    (parse_input stdin) (output_callback None)
	    std_world_printer 0
      | RM_MATCH (p1, p2) ->
	  let p1_in_channel, p1_out_channel = Unix.open_process (p1 ^ " 0")
	  and p2_in_channel, p2_out_channel = Unix.open_process (p2 ^ " 1")
	  in
	    play_game context (create_default_world ())
	      (parse_input p1_in_channel) (output_callback (Some p1_out_channel))
	      (parse_input p2_in_channel) (output_callback (Some p2_out_channel))
	      std_world_printer 1

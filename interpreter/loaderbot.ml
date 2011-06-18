open Inter
open Parser
open Printer
open Arg
open Cards
open Bot

type privdata = {
  pd_turns : turn list;
  pd_stage : int
}

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

let move_callback context world proponent_move privdata =
  let move, rest, stage =
    match privdata.pd_stage with
      | 0 (* kill enemy field 0, not yet implemented *)
      | 1 -> (* apply commands from file *)
	  begin
	    match privdata.pd_turns with
	      | turn :: [] -> turn, [], 2
	      | turn :: rest -> turn, rest, 1
	      | _ -> failwith "should not happen"
	  end
      | _ -> Left (I, 0), [], 2
  in
    move, { pd_turns = rest; pd_stage = stage }

let _ =
  let ifi = open_in "/icfpnfs/SCHANI/killer-fn.cmd"
  and skipfirst = match Sys.argv with
    | [| _; "0" |] -> false
    | [| _; "1" |] -> true
    | _ -> failwith "dunno what to do, gimme some args"
  in let turns = read_turns_from_file ifi quiet_printer
  in let pd = { pd_turns = turns; pd_stage = 0 }
  in
    bootloop ~skipfirst move_callback pd

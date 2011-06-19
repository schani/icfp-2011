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

let move_callback context world proponent_move turn_stats privdata =
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
  let turns = read_turns_from_file "/icfpnfs/SCHANI/killer-fn.cmd"
  in let pd = { pd_turns = turns; pd_stage = 0 }
  in
    bootloop move_callback pd

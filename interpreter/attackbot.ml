open Inter
open Parser
open Printer
open Arg
open Cards
open Bot
open Cofu

type privdata = {
  pd_turns: turn list;
  pd_stage: int;
  pd_victim: int;
}

let move_callback context world proponent_move turn_stats privdata =
  try
    let move, rest, victim, stage =
      match privdata.pd_stage with
	| 0 (* kill enemy field 0, not yet implemented *)
	| 1 -> begin (* apply commands from file *)
	    match privdata.pd_turns with
	      | turn :: [] -> turn, [], 0, 2
	      | turn :: rest -> turn, rest, 0, 1
	      | _ -> failwith "should not happen"
	  end
	| 2 -> begin (* search for cool *)
	    let victim = (255 - (last_alive_other_slot world))
	    in let job = write_number_to_slot victim 64
	    in
	      match job with
		| jobfirst :: jobrest ->
		    jobfirst, jobrest, victim, 3
		| [] -> raise (Bot_error
				 "move_callback: write_number_to_slot failed")
	  end
	| 3 -> begin (* apply commands from file *)
	    match privdata.pd_turns with
	      | turn :: [] -> turn, [], privdata.pd_victim, 4
	      | turn :: rest -> turn, rest, privdata.pd_victim, 3
	      | _ -> failwith "should not happen"
	  end
	| 4 -> begin (* start the action *)
	    Right (65, I), [], privdata.pd_victim, 5
	  end
	| 5 -> (* search for new victim if already killed *)
	    let victim =  privdata.pd_victim
	    in let vicvit, _ = context.read_other_vit victim world
	    in
	      if vicvit > 0 then (* victim still alive *)
		Right (65, I), [], victim, 5
	      else (* victim killed, we need new target *)
		  let next_victim = (255 - (last_alive_other_slot world))
		  in let possible_job = write_number_to_slot next_victim 64
		  in
		    if (List.length possible_job) < (next_victim - victim) then (* new number *)
		      match possible_job with
			| jobfirst :: jobrest ->
			    jobfirst, jobrest, next_victim, 3
			| [] -> raise (Bot_error
					 "move_callback: write_number_to_slot failed")
		    else (* reachable by suckas *)
		      let sucka = Left (Succ, 64)
		      in let suckas = (Array.to_list (Array.make (next_victim - victim - 1) sucka))
		      in
			sucka, suckas, next_victim, 3
	| _ -> raise (Bot_error "move_callback got illegal state")
    in
      move, { pd_turns = rest; pd_stage = stage; pd_victim = victim }
  with
      Bot_error str ->
	Printf.fprintf stderr "move_callback: Bot_error: %s\n" str;
	Left (I, 0), privdata

let _ =
  let turns = read_turns_from_file "functions/masr.cmd"
  in let pd = { pd_turns = turns; pd_stage = 0; pd_victim = 0}
  in
    bootloop move_callback pd

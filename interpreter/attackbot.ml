open Inter
open Parser
open Printer
open Arg
open Cards
open Bot
open Cofu

type state =
  | S_FIND_VICTIM of int
  | S_APPLY_TURNS of (turn list * state)
  | S_MASR_LAUNCH_ATTACK of int
  | S_MASR_FIND_NEXT_VICTIM of int

type privdata = {
  pd_state: state
}


let calculate_64_er_slot_value slot_to_attack =
  255 - slot_to_attack

let move_callback context world proponent_move turn_stats privdata =
  try
    let rec state_machine state =
      match privdata.pd_state with
	| S_FIND_VICTIM limit ->
	    let victim = (last_alive_other_slot_custom world limit)
	    in let job = write_number_to_slot (calculate_64_er_slot_value victim) 64
	    in
	      state_machine (S_APPLY_TURNS (job, S_MASR_LAUNCH_ATTACK victim))
	| S_APPLY_TURNS (turns, new_state) -> begin
	    match turns with
	      | turn :: [] -> turn, new_state
	      | turn :: rest -> turn, S_APPLY_TURNS (rest, new_state)
	      | _ -> failwith "should not happen"
	  end
	| S_MASR_LAUNCH_ATTACK victim ->
	    let vicvit, _ = context.read_other_vit victim world
	    in
	      if vicvit > 0 then
		Right (victim, I), S_MASR_LAUNCH_ATTACK victim
	      else
		state_machine (S_MASR_FIND_NEXT_VICTIM victim)
	| S_MASR_FIND_NEXT_VICTIM old_victim ->
	    let next_victim = last_alive_other_slot world
	    in let possible_job = write_number_to_slot (calculate_64_er_slot_value next_victim) 64
	    in
	      if (List.length possible_job) < (old_victim - next_victim) then (* create new number *)
		state_machine (S_FIND_VICTIM next_victim)
	      else
		let new_vic_fun = Left (Succ, 64)
		in let new_vic_funs = (Array.to_list (Array.make (old_victim - next_victim) new_vic_fun))
		in
		  state_machine (S_APPLY_TURNS (new_vic_funs, S_MASR_LAUNCH_ATTACK next_victim))
    in let move, next_state = state_machine privdata.pd_state
    in
	move, { pd_state = next_state }
  with
      Bot_error str ->
	Printf.fprintf stderr "move_callback: Bot_error: %s\n" str;
	Left (I, 0), privdata

let _ =
  let turns = read_turns_from_file "functions/masr.cmd"
  in let pd = { pd_state = S_APPLY_TURNS (turns, S_FIND_VICTIM 255) }
  in
    bootloop move_callback pd

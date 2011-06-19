open Inter
open Parser
open Printer
open Arg
open Cards
open Bot
open Cofu
open Printf

type state =
  | S_APPLY_TURNS of (turn list * state)
  | S_MASR_FIND_VICTIM of (default_world_type -> int)
  | S_MASR_LAUNCH_ATTACK of int
  | S_MASR_FIND_NEXT_VICTIM of int
  | S_RESURRECT of (int list * state)

type privdata = {
  pd_state: state;
  during_load_slots: int list;
  post_load_slots: int list;
}
    
let calculate_64_er_slot_value slot_to_attack =
  255 - slot_to_attack

let rec find_dead_slots ?(result=[]) slots = function
  | [] -> result
  | slotnr :: rest ->
      if (fst slots.(0)) <= 0 then
	find_dead_slots ~result:(slotnr :: result) slots rest
      else
	find_dead_slots ~result slots rest

let generate_ressurect_code world deadnr =
  let alive_slot = last_alive_slotnr (fst world) 255
  in let numgen = write_number_to_slot deadnr alive_slot
  in
    List.append numgen [(Left (Revive, alive_slot))]

let botdebug str =
  output_string stderr ("% " ^ str);
  flush stderr

let turns_masr = read_turns_from_file "functions/masr-65-64-init.cmd"
let turns_masr4 = read_turns_from_file "functions/masr4.cmd"

let move_callback context world proponent_move turn_stats privdata =
  try
    let rec state_machine state privdata =
      match state with
	| S_RESURRECT (deads, stored_state) -> begin
	    match deads with
	      | [] -> state_machine stored_state privdata
	      | x :: rest ->
		  let job = generate_ressurect_code world x
		  in
		    state_machine (S_APPLY_TURNS (job, S_RESURRECT (rest, stored_state))) privdata
	  end
	| S_APPLY_TURNS (turns, stored_state) -> begin
	    botdebug (sprintf "move_callback: S_APPLY_TURNS: %i left\n" (List.length turns));
	    let deads = find_dead_slots (fst world) privdata.during_load_slots
	    in
	      if (List.length deads) > 0 then
		(* oh no, we have been interrupted *)
		state_machine (S_RESURRECT (deads, S_APPLY_TURNS (turns_masr, S_MASR_FIND_VICTIM biggest_other_slot)))
		  privdata
	      else
		match turns with
		  | turn :: [] -> turn, stored_state, privdata
		  | turn :: rest -> turn, S_APPLY_TURNS (rest, stored_state), privdata
		  | [] -> state_machine stored_state privdata
	  end
	| S_MASR_FIND_VICTIM selector ->
	    (* botdebug ("VORM DEPPATEN SCHASS\n");*)
	    let victim = selector world
	    in
	      botdebug (sprintf "move_callback: S_MASR_FIND_VICTIM: %i\n" victim);
	      let job = write_number_to_slot (calculate_64_er_slot_value victim) 64
	      in
		state_machine (S_APPLY_TURNS (job, (S_MASR_LAUNCH_ATTACK victim))) privdata
	| S_MASR_LAUNCH_ATTACK victim ->
	    botdebug (sprintf "move_callback: S_MASR_LAUNCH_ATTACK: attacking %i\n" victim);
	    (
	      let expr, _ = context.read_other_field victim world
	      and vit, _ = context.read_other_vit victim world
	      in
		botdebug (sprintf "slot %i field=%s vit=%i\n" victim (string_of_expr expr) vit)
	    );
	    let vicvit, _ = context.read_other_vit victim world
	    in
	      if vicvit > 0 then
		Right (65, I), S_MASR_LAUNCH_ATTACK victim, privdata
	      else
		state_machine (S_MASR_FIND_NEXT_VICTIM victim) privdata
	| S_MASR_FIND_NEXT_VICTIM old_victim ->
	    botdebug (sprintf "move_callback: S_MASR_FIND_NEXT_VICTIM: old_victim=%i\n" old_victim);
	    if old_victim <= 0 then
	      state_machine (S_MASR_FIND_VICTIM (function _ -> 255)) privdata
	    else
	      let next_victim = old_victim - 1
	      in let possible_job = write_number_to_slot (calculate_64_er_slot_value next_victim) 64
	      in
		if (List.length possible_job) < (old_victim - next_victim) then (* create new number *)
		  state_machine (S_MASR_FIND_VICTIM (function _ -> next_victim)) privdata
		else
		  let new_vic_fun = Left (Succ, 64)
		  in let new_vic_funs = (Array.to_list (Array.make (old_victim - next_victim) new_vic_fun))
		  in
		    state_machine (S_APPLY_TURNS (new_vic_funs, S_MASR_LAUNCH_ATTACK next_victim)) privdata
    in let move, next_state, privdata = state_machine privdata.pd_state privdata
    in
      move, { privdata with pd_state = next_state }
  with
      Bot_error str ->
	botdebug ("move_callback: Bot_error: %s\n" ^ str);
	Left (I, 0), privdata

let _ =
  let pd = {
    pd_state = S_APPLY_TURNS (turns_masr, S_MASR_FIND_VICTIM biggest_other_slot);
    during_load_slots = [0; 1; 2; 3; 64; 65];
    post_load_slots = [64; 65];
  }
  in
    bootloop move_callback pd

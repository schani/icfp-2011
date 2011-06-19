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
  | S_APPLY_ATTACK of (turn list * state)
  | S_MASR_FIND_VICTIM of (default_world_type -> int)
  | S_MASR_LAUNCH_ATTACK of int
  | S_MASR_FIND_NEXT_VICTIM of int
  | S_RESURRECT of (int list * state)
  | S_APPLY_RESURRECT_TURNS of (turn list * state)
  | S_HEAL_4 of state

type privdata = {
  pd_state: state;
  during_load_slots: int list;
  post_load_slots: int list;
}

let strjoin l = List.fold_right (fun a b -> (a ^ "; " ^ b)) (List.map string_of_int l) ""
let botdebug str =
  output_string stderr ("% " ^ str);
  flush stderr

let calculate_4_er_slot_value slot_to_attack =
  (255 - slot_to_attack)

let rec find_dead_slots ?(result=[]) slots = function
  | [] ->
      if (List.length result) > 0 then begin
	botdebug (sprintf "find_dead_slots: WE ARE DEAD NOW: first=%s\n" (strjoin result));
	result
      end else
	result
  | slotnr :: rest ->
      if (fst slots.(slotnr)) <= 0 then
	find_dead_slots ~result:(slotnr :: result) slots rest
      else
	find_dead_slots ~result slots rest

let is_slot4_ok context world =
  ((fst (context.read_own_vit 4 world)) > 8192)

let generate_ressurect_code world deadnr =
  let alive_slot = last_alive_slotnr (fst world) 255
  in let numgen = write_number_to_slot deadnr alive_slot
  in
    List.append numgen [(Left (Revive, alive_slot))]

let rec genhelpnums ?(res=[]) = function
  | 256 -> List.rev res
  | i -> genhelpnums ~res:(i :: res) (i + 1)
      
let turns_masr = read_turns_from_file "functions/masr-8-4-noinit.cmd"
let turns_masr4 = read_turns_from_file "functions/masr4.cmd"
let turns_helper4 =
  Array.of_list (List.map (fun i -> read_turns_from_file (sprintf "functions/help-x-to-4/help-%i-to-4.cmd" i))
		     (genhelpnums 13))

let move_callback context world proponent_move turn_stats privdata =
  try
    let rec state_machine state privdata =
      match state with
	| S_RESURRECT (deads, stored_state) -> begin
	    botdebug (sprintf "move_callback: S_RESURRECT\n");
	    match deads with
	      | [] -> state_machine stored_state privdata
	      | x :: rest ->
  		  let job = generate_ressurect_code world x
		  in
		    state_machine (S_APPLY_RESURRECT_TURNS (job, S_RESURRECT (rest, stored_state))) privdata
	  end
	| S_APPLY_RESURRECT_TURNS (turns, stored_state) -> begin
	    botdebug (sprintf "move_callback: S_APPLY_RESURRECT_TURNS: %i left\n" (List.length turns));
	    match turns with
	      | turn :: [] -> turn, stored_state, privdata
	      | turn :: rest -> turn, S_APPLY_RESURRECT_TURNS (rest, stored_state), privdata
	      | [] -> state_machine stored_state privdata
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
	| S_APPLY_ATTACK (turns, stored_state) -> begin
	    botdebug (sprintf "move_callback: S_APPLY_ATTACK: %i left\n" (List.length turns));
	    let deads = find_dead_slots (fst world) privdata.post_load_slots
	    in
	      if (List.length deads) > 0 then
		(* oh no, we have been interrupted *)
		state_machine (S_RESURRECT (deads, S_APPLY_TURNS (turns_masr, S_MASR_FIND_VICTIM biggest_other_slot)))
		  privdata
	      else
		match turns with
		  | turn :: [] -> turn, stored_state, privdata
		  | turn :: rest -> turn, S_APPLY_ATTACK (rest, stored_state), privdata
		  | [] -> state_machine stored_state privdata
	  end
	| S_MASR_FIND_VICTIM selector ->
	    (* botdebug ("VORM DEPPATEN SCHASS\n");*)
	    let victim = selector world
	    in
	      botdebug (sprintf "move_callback: S_MASR_FIND_VICTIM: %i\n" victim);
	      let job = write_number_to_slot (calculate_4_er_slot_value victim) 4
	      in
		state_machine (S_APPLY_TURNS (job, (S_MASR_LAUNCH_ATTACK victim))) privdata
	| S_MASR_LAUNCH_ATTACK victim ->
	    botdebug (sprintf "move_callback: S_MASR_LAUNCH_ATTACK: attacking %i\n" victim);
	    let deads = find_dead_slots (fst world) privdata.post_load_slots
	    in
	      if (List.length deads) > 0 then
		state_machine (S_RESURRECT (deads, S_APPLY_ATTACK (turns_masr, S_MASR_FIND_VICTIM biggest_other_slot)))
		  privdata
	      else begin
		(
		  let expr, _ = context.read_other_field victim world
		  and vit, _ = context.read_other_vit victim world
		  in
		    botdebug (sprintf "slot %i field=%s vit=%i\n" victim (string_of_expr expr) vit)
		);
		let vicvit, _ = context.read_other_vit victim world
		in
		  if vicvit > 0 then
		    Right (8, I), S_MASR_LAUNCH_ATTACK victim, privdata
		  else
		    state_machine (S_MASR_FIND_NEXT_VICTIM victim) privdata
	      end
	| S_MASR_FIND_NEXT_VICTIM old_victim ->
	    botdebug (sprintf "move_callback: S_MASR_FIND_NEXT_VICTIM: old_victim=%i\n" old_victim);
	    let deads = find_dead_slots (fst world) privdata.post_load_slots
	    in
	      if (List.length deads) > 0 then
		state_machine (S_RESURRECT (deads, S_APPLY_TURNS (turns_masr, S_MASR_FIND_VICTIM biggest_other_slot)))
		  privdata
	      else begin
		if old_victim <= 0 then
		  state_machine (S_MASR_FIND_VICTIM (function _ -> 255)) privdata
		else
		  let next_victim = old_victim - 1
		  in let possible_job = write_number_to_slot (calculate_4_er_slot_value next_victim) 4
		  in
		    if (List.length possible_job) < (old_victim - next_victim) then (* create new number *)
		      state_machine (S_MASR_FIND_VICTIM (function _ -> next_victim)) privdata
		    else
		      let new_vic_fun = Left (Succ, 4)
		      in let new_vic_funs = (Array.to_list (Array.make (old_victim - next_victim) new_vic_fun))
		      in
			state_machine (S_APPLY_ATTACK (new_vic_funs, S_MASR_LAUNCH_ATTACK next_victim)) privdata
	      end
	| S_HEAL_4 stored_state ->
	    if is_slot4_ok context world then
	      state_machine stored_state privdata
	    else begin
	      match find_healing_slot world with
		| None -> state_machine stored_state privdata
		| Some i -> state_machine (S_APPLY_TURNS (turns_helper4.(i), stored_state)) privdata
	    end
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
				(* (function _ -> 0)); *)
    during_load_slots = [0; 1; 2; 3; 8; 12];
    post_load_slots = [4; 8; 12];
  }
  in
    bootloop move_callback pd

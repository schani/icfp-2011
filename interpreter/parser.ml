open Cards
open Printer

exception Parse_error of string

let parse_card = function
  | "I" -> I
  | "zero" -> Zero
  | "succ" -> Succ
  | "dbl" -> Dbl
  | "get" -> Get
  | "put" -> Put
  | "S" -> S
  | "K" -> K
  | "inc" -> Inc
  | "dec" -> Dec
  | "attack" -> Attack
  | "help" -> Help
  | "copy" -> Copy
  | "revive" -> Revive
  | "zombie" -> Zombie
  | _ -> raise (Parse_error "unknown card name")

let parse_slot str =
  try
    let slot = int_of_string str
    in
      if slot >= 0 && slot <= 255 then
	slot
      else
	raise (Parse_error "slot out of range")
  with
      _ ->
	raise (Parse_error "exception while parsing slot")

let parse_input handle printer () =
  let first_line = try
    printer MsgQuestionMove;
    input_line handle
  with
      End_of_file -> raise End_of_file
  in
    try
      match first_line with 
	  "1" ->
	    printer MsgQuestionCard;
	    let card = parse_card (input_line handle)
	    in
	      printer MsgQuestionSlot;
	      let slot = parse_slot (input_line handle)
	      in
		Left (card, slot)
	| "2" -> 
	    printer MsgQuestionSlot;
	    let slot = parse_slot (input_line handle)
	    in
	      printer MsgQuestionCard;
	      let card = parse_card (input_line handle)
	      in
		Right (slot, card)
	| _ ->
	    let errmsg = "expecting [12] but got " ^ first_line
	    in
	      raise (Parse_error errmsg)
    with
      | End_of_file ->
	  raise (Parse_error "End_of_file where it should happen")

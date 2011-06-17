type cards = | I | Zero | Succ | Dbl | Get | Put | S | K 
	     | Inc | Dec | Attack | Help | Copy | Revive | Zombie 
  
type parsed_turn =
  | Left of (cards * int)
  | Right of (int * cards)

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

let parse_input () =
  let first_line = try
    input_line stdin
  with
      End_of_file -> raise End_of_file
  in
    try
      match first_line with 
	  "1" ->
	    let card = parse_card (input_line stdin)
	    in let slot = parse_slot (input_line stdin)
	    in
	      Left (card, slot)
	| "2" -> 
	    let slot = parse_slot (input_line stdin)
	    in let card = parse_card (input_line stdin)
	    in
	      Right (slot, card)
	| _ ->
	    let errmsg = "expecting [12] but got " ^ first_line
	    in
	      raise (Parse_error errmsg)
    with
      | End_of_file ->
	  raise (Parse_error "End_of_file where it should happen")
      | _ ->
	  raise (Parse_error "unknown error")

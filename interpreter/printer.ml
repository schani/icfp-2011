open Cards
open Printf

type msg_type =
  | MsgStartup
  | MsgTurn of int
  | MsgPlayer of int
  | MsgWorld of default_world_type
  | MsgQuestion
  | MsgMove of (int * turn)

let string_of_card = function
  | I-> "I"
  | Zero -> "zero"
  | Succ -> "succ"
  | Dbl -> "dbl"
  | Get -> "get"
  | Put -> "put"
  | S -> "S"
  | K -> "K"
  | Inc -> "inc"
  | Dec -> "dec"
  | Attack -> "attack"
  | Help -> "help"
  | Copy -> "copy"
  | Revive -> "revive"
  | Zombie -> "zombie"

let msg_of_turn = function
  | Left (card, slot) ->
      sprintf "1\n%s\n%i\n" (string_of_card card) slot
  | Right (slot, card) ->
      sprintf "2\n%i\n%s\n" slot (string_of_card card)

let string_of_turn = function
  | Left (card, slot) ->
      sprintf "applied card %s to slot %i" (string_of_card card) slot
  | Right (slot, card) ->
      sprintf "applied slot %i to card %s" slot (string_of_card card)

let rec string_of_expr = function
  | Card c -> string_of_card c
  | Num i -> string_of_int i
  | Lambda (e1, e2) -> "(" ^ (string_of_expr e1) ^ (string_of_expr e2) ^ ")"
  | Error -> "Error"
  | Sf e -> "S(" ^ (string_of_expr e) ^ ")"
  | Sfg (e1, e2) -> "S(" ^ (string_of_expr e1) ^ (string_of_expr e2) ^ ")"
  | Kx e -> "K(" ^ (string_of_expr e) ^ ")"
  | AttackI e -> "Attack(" ^ (string_of_expr e) ^ ")"
  | AttackIJ (e1, e2) ->
      "Attack(" ^ (string_of_expr e1) ^ (string_of_expr e2) ^ ")"
  | HelpI e -> "HelpI(" ^ (string_of_expr e) ^ ")"
  | HelpIJ (e1, e2) -> "Help(" ^ (string_of_expr e1) ^ (string_of_expr e2) ^ ")"
  | ZombieI e -> "Zombie(" ^ (string_of_expr e) ^ ")"

let print_slot i = function
  | 10000, Card I -> ()
  | vir, expr -> printf "%i={%i,%s}\n" i vir (string_of_expr expr)

let std_world_printer msg =
  begin
    match msg with
      | MsgStartup -> print_string "Ocaml: The Gathering\n"
      | MsgTurn i -> printf "###### turn %i\n" i
      | MsgPlayer i -> printf "*** player %i's turn, with slots:\n" i
      | MsgWorld world ->
	  Array.iteri print_slot (fst world);
	  print_string "(slots {10000,I} are omitted)\n";
      | MsgQuestion ->
	  print_string "(1) apply card to slot, or (2) apply slot to card?\n"
      | MsgMove (i, turn) ->
	  printf "player %i %s\n" i (string_of_turn turn)
  end;
  flush stdout

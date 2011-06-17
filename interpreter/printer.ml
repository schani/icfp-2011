open Cards
open Printf

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

let string_of_turn = function
  | Left (card, slot) ->
      sprintf "1\n%s\n%i\n" (string_of_card card) slot
  | Right (slot, card) ->
      sprintf "2\n%i\n%s\n" slot (string_of_card card)

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

let rec print_slots ?(i=0) = function
  | [] ->
      print_string "(slots {10000,I} are ommited)\n"
  | (vir, expr) :: rest ->
      printf "%i={%i,%s}\n" i vir (string_of_expr expr);
      print_slots ~i:(i+1) rest

(* world is of type (slot array)*(slot array) *)
let std_world_printer slots =
  print_slots slots
  

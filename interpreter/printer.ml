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

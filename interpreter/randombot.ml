open Bot
open Cards
open Inter
open Parser
open Printer

let random_card () =
  let all_cards = [| I; Zero; Succ; Dbl; Get; Put; S; K;
		     Inc; Dec; Attack; Help; Copy; Revive; Zombie |]
  in
    all_cards.(Random.int (Array.length all_cards))
and random_slot () =
  Random.int 256

let move_callback context world proponent_move privdata =
  match Random.bool () with
    | true ->  Left (random_card (), random_slot ()), ()
    | false -> Right (random_slot (), random_card()), ()

let _ =
  Random.self_init ();
  bootloop move_callback ()


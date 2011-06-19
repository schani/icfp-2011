open Bot
open Cards
open Inter
open Parser
open Printer

let move_callback context world proponent_move privdata =
  match proponent_move with
    | None -> Left (I, 0), ()
    | Some move -> move, ()

let _ =
  bootloop move_callback ()

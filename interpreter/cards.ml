type cards = | I | Zero | Succ | Dbl | Get | Put | S | K 
	     | Inc | Dec | Attack | Help | Copy | Revive | Zombie 
  
type turn =
  | Left of (cards * int)
  | Right of (int * cards)
      
type skiexpr = 
  | Card of cards
  | Num of int
  | Lambda of (skiexpr * skiexpr)
  | Error 
  | Sf of skiexpr
  | Sfg of (skiexpr * skiexpr)
  | Kx of skiexpr
  | AttackI of skiexpr
  | AttackIJ of (skiexpr * skiexpr)
  | HelpI of skiexpr
  | HelpIJ of (skiexpr * skiexpr)
  | ZombieI of skiexpr

type slot = int * skiexpr
type default_world_type = (slot array)*(slot array)

type move_stats = {
  suppress_warning: bool;
  reset_slots: int list
}

type turn_stats = move_stats * move_stats

type slotallocation =
  | SA of (int * skiexpr)
      
type compositexpr =
  | CCard of cards
  | CNum of int
  | CLambda of (compositexpr * compositexpr)
  | Compose of (compositexpr * slotallocation) 
      
let empty_move_stats () =
   {
     suppress_warning = true;
     reset_slots = [];
   }

let empty_turn_stats () = (empty_move_stats ()), (empty_move_stats ())

let rec sizeof_skiexpr = function
  | Card _
  | Num _ -> 1
  | Lambda (e1, e2) -> (sizeof_skiexpr e1) + (sizeof_skiexpr e2)
  | _ ->
      Printf.fprintf stderr "%% WARNING: sizeof_skiexpr called on nund CML stuff\n";
      0

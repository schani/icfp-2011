type cards = | I | Zero | Succ | Dbl | Get | Put | S | K 
	     | Inc | Dec | Attack | Help | Copy | Revive | Zombie 
  
type turn =
  | Left of (cards * int)
  | Right of (int * cards)

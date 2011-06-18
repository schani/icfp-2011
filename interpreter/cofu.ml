(* searches for occurences of slotexpr in tree
 * returns list of nodes
 *)

(*

type cards = | I | Zero | Succ | Dbl | Get | Put | S | K 
	     | Inc | Dec | Attack | Help | Copy | Revive | Zombie 

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

let mytree = Lambda (Lambda (Card Zero, Num 0), Num 3)
let myslot = Lambda (Card Zero, Num 0)

*)

let rec find_subexpressions slotexpr tree =
  if slotexpr = tree then
    [tree]
  else
    match tree with
      | Card _
      | Num _ -> []
      | Lambda (left, right) ->
	  List.append (find_subexpressions slotexpr left) (find_subexpressions slotexpr right)
      | _ -> failwith "find_subexpression got illegal skiexpr"

let find_subexpressions_in_slots slots tree =
  Array.map (fun (_, slotexpr) -> find_subexpressions slotexpr tree) slots

(* let build_function_in_world ski world = *)

let is_primitive = function
  | Card _
  | Num _ -> true
  | _ -> false
    
let rec generify_skiexpr = function
  | Card c -> Card c
  | Num n -> Num n
  | Lambda (Lambda (a,b), Lambda (c,d))
  | Lambda (l, r) -> Lambda (generify_skiexpr l, generify_skiexpr r)
  | _ -> failwith "generify_skiexpr got illegal skiexpr"
      
      
;;
;;


Lambda( Lambda(a,b), Lambda(c,d)) -> Lambda( Lambda( Card S, Lambda( Card K

						Lambda(a,b),

						Lambda(c,d)


  Lambda(Card K,Num 10)

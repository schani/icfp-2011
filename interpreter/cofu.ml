open Cards
open List


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

(* let rec find_subexpressions slotexpr tree = *)
(*   if slotexpr = tree then *)
(*     [tree] *)
(*   else *)
(*     match tree with *)
(*       | Card _ -> [] *)
(*       | Num _ -> [] *)
(*       | Lambda (left, right) -> *)
(* 	  List.append (find_subexpressions slotexpr left) (find_subexpressions slotexpr right) *)
(*       | _ -> failwith "find_subexpression got illegal skiexpr" *)

(* let find_subexpressions_in_slots slots tree = *)
(*   Array.map (fun (_, slotexpr) -> find_subexpressions slotexpr tree) slots *)

(* (\* let build_function_in_world ski world = *\) *)


let is_primitive = function
  | Card _
  | Num _ -> true
  | _ -> false
    












let rec generify_skiexpr = function
  | Card c -> CCard c
  | Num n -> CNum n
  | Lambda (Lambda (a,b), Lambda (c,d)) ->
      Compose(
	CLambda(
	  CLambda(
	    CCard S,
	    CLambda(
	      CLambda(
		CCard K,
		generify_skiexpr  (Lambda (a, b))
	      ),
	      CCard Get
	    )
	  ),
	  CCard Zero
	),
	SA (0,Lambda (c, d ))
      )
  | Lambda (l, r) -> CLambda (generify_skiexpr l, generify_skiexpr r)
  | _ -> failwith "generify_skiexpr got illegal skiexpr"



let erase s =
  [ (Left (Put, s)); (Right (s, Zero))]



let rec write_number_to_slot x s =
  let halfx = ( x / 2) 
  in
  if x = 0 then
    erase s
  else
    (append 
      (write_number_to_slot halfx s)
      (append
	(if not (halfx = 0) then
	  [(Left (Dbl, s))]
	else
	  []
	)
	(if (x mod 2) = 1 then
	  [(Left (Succ, s))]
	else
	  []
	)
      )
    )


(* let rec make_moves expr target free =   *)
(*   match expr   *)
(*   | CNum n -> construct n target  *)
(*   | CCard c -> [] *)
(*   | CLambda (a,b) ->     *)


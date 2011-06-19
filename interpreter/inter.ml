(*
type cards = | I | Zero | Succ | Dbl | Get | Put | S | K 
	     | Inc | Dec | Attack | Help | Copy | Revive | Zombie 
*)
open Cards
open Printer
  
(*		 
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
*)
type liveness = Dead | Alive

type 'world intercontext = {life: liveness;
			    depth: int;
			    read_own_vit: int -> 'world -> (int * 'world);
			    read_other_vit: int -> 'world -> (int * 'world); 
			    read_own_field: int -> 'world ->  (skiexpr * 'world);
			    read_other_field: int -> 'world -> (skiexpr * 'world);
			    write_own_vit: int -> int -> 'world -> 'world;
			    write_other_vit: int -> int -> 'world -> 'world;
			    write_own_field: int -> skiexpr -> 'world -> 'world;
			    write_other_field: int -> skiexpr -> 'world -> 'world;
			    count_alive_own: 'world -> (int * 'world);
			    count_alive_other: 'world -> (int * 'world);
			    error: string -> 'world -> unit;
			    end_move: 'world -> turn_stats -> ('world * turn_stats);
			   } 

let rec plus l x y = 
  match l with 
    | Dead -> 
      minus Alive x y
    | Alive -> 
	let r = x + y in 
	if r > 65535 then
	  65535
	else 
	  r
and minus l x y = 
  match l with
    | Dead ->
      plus Alive x y
    | Alive ->
      let r = x - y in 
      if r < 0 then 
	0
      else
	r
and decr l x = 
  match l with
    | Dead -> 
      plus Alive x 1
    | Alive -> 
      let r = x - 1 in
      if r < -1 then
	-1
      else
	r

let error context message world = 
  ignore(context.error message world);
(*  assert false; *)
  Error,world

let isnum x = match x with
  | Card(Zero) | Num _ -> true
  | _ -> false

let getnum x world context = match x with 
  | Card(Zero) -> 0,world
  | Num x -> x,world
  | _ -> 
    ignore(error context "not number" world); 
(*         assert false; *)
    0,world
    
let isslot = function 
  | Card(Zero) -> true
  | Num x -> (x >= 0) && (x<=255)
  | _ -> false

let isalive x = (x > 0)
let iszombie x = (x == -1)

let incrdepth c = 
  let d = c.depth + 1 in
  let c = {c with depth = d} in
  c,(d >= 1000)

let rec inter context world expr = 
  (* Printf.printf "INTER: %s\n" (string_of_expr expr); *)
  let context,stop = incrdepth context in
  if stop then
    error context "max depth exceeded" world
  else match expr with
  | Num _ as x -> x,world
  | Card(Zero) -> Num 0,world
  | Card(_) as c -> c,world (* error context "only zero can be played as a value" world *)

  | Lambda(Card(I),x) -> x,world
  | Lambda(Card(Zero),_) -> error context "zero is not a function" world
  | Lambda(Card(Succ),n) -> let n,world = inter context world n in let n,world = getnum n world context in Num (plus Alive n 1),world
  | Lambda(Card(Dbl),n) ->  let n,world = inter context world n in let n,world = getnum n world context in Num (plus Alive n n),world

  | Lambda(Card(Get),x) -> (
    let s,world = inter context world x in
    if isslot s then 
      let s,world = getnum s world context in
      let vit,world = context.read_own_vit s world in
      if isalive vit then 
	let sv,world = context.read_own_field s world in
	sv,world
      else
	error context "get: is dead" world
    else
      error context "get: not slot" world)
  | Lambda(Card(Put),x) -> Card(I),world

  | Lambda(Card(S),f) -> let f,world = inter context world f in Sf(f),world
  | Lambda(Sf(f),g) -> let g,world = inter context world g in Sfg(f,g),world
  | Lambda(Sfg(f,g),x) -> 
    let h,world = inter context world (Lambda(f,x)) in
    let y,world = inter context world (Lambda(g,x)) in
    let z,world = inter context world (Lambda(h,y)) in
    z,world

  | Lambda(Card(K),x) -> let x,world = inter context world x in Kx(x),world
  | Lambda(Kx(x),y) -> x,world

  | Lambda(Card(Inc),x) -> (
    let s,world = inter context world x in
    if isslot s then 
      let s,world = getnum s world context in
      let vit,world = context.read_own_vit s world in
      if isalive vit then
	let vit = plus context.life vit 1 in
	Card(I),context.write_own_vit s vit world
      else
	Card(I),world
    else
      error context "inc: not slot" world)

  | Lambda(Card(Dec),x) -> (
    let s,world = inter context world x in
    if isslot s then 
      let s,world = getnum s world context in
      let vit,world = context.read_other_vit (255 - s) world in
      if isalive vit then
	let vit = decr context.life vit in
	Card(I),context.write_other_vit (255 - s) vit world
      else
	Card(I),world
    else
      error context "dec: not slot" world)

  | Lambda(Card(Attack),i) -> let i,world = inter context world i in AttackI(i),world
  | Lambda(AttackI(i),j) -> let j,world = inter context world j in AttackIJ(i,j),world
  | Lambda(AttackIJ(i,j),n) -> 
    let n,world = inter context world n in 
    if isnum n then
      let n,world = getnum n world context in
      if isslot i then 
	let i,world = getnum i world context in
	let vit,world = context.read_own_vit i world in
	let vit = minus Alive vit n in
	if isalive vit then
	  let world = context.write_own_vit i vit world in
	  let n = (n*9)/10 in
	  if isslot j then
	    let j,world = getnum j world context in
	    let vit,world = context.read_other_vit (255 - j) world in
	    let vit = minus context.life vit n in
	    let world = context.write_other_vit (255 - j) vit world in
	    Card(I),world
	  else
	    error context "attack: not slot j" world
	else
	  error context "attack: not alive i (enough)" world
      else
	error context "attack: not slot i" world
    else
      error context "attack: not num n" world

  | Lambda(Card(Help),i) -> let i,world = inter context world i in HelpI(i),world
  | Lambda(HelpI(i),j) -> let j,world = inter context world j in HelpIJ(i,j),world
  | Lambda(HelpIJ(i,j),n) -> 
    let n,world = inter context world n in 
    if isnum n then
      let n,world = getnum n world context in 
      if isslot i then 
	let i,world = getnum i world context in
	let vit,world = context.read_own_vit i world in
	let vit = minus Alive vit n in
	if isalive vit then
	  let world = context.write_own_vit i vit world in
	  let n = (n*11)/10 in
	  if isslot j then
	    let j,world = getnum j world context in
	    let vit,world = context.read_own_vit j world in
	    let vit = plus context.life vit n in
	    let world = context.write_own_vit j vit world in
	    Card(I),world	    
	  else
	    error context "attack: not slot j" world
	else
	  error context "attack: not alive i (enough)" world
      else
	error context "attack: not slot i" world
    else
      error context "attack: not num n" world

  | Lambda(Card(Copy),i) -> (
    let s,world = inter context world i in
    if isslot s then
      let s,world = getnum s world context in
      context.read_other_field s world
    else 
      error context "copy: not slot" world)

  | Lambda(Card(Revive),i) -> (
    let s,world = inter context world i in
    if isslot s then 
      let s,world = getnum s world context in
      let vit,world = context.read_own_vit s world in
      if isalive vit then
	Card(I),world
      else
	Card(I),context.write_own_vit s 1 world
    else
      error context "revive: not slot" world)

  | Lambda(Card(Zombie),i) -> let i,world = inter context world i in ZombieI(i),world
  | Lambda(ZombieI(i),x) -> (
    if isslot i then 
      let i,world = getnum i world context in
      let vit,world = context.read_own_vit i world in
      if isalive vit then
	error context "zombie: cannot zombie alive slot" world
      else
	Card(I),context.write_other_field i x world
    else
      error context "zombie: not slot" world)

  | Lambda(Num(_),_) -> error context "cannot call num" world
  | Lambda(Error(_),_) -> error context "cannot call error" world
  | Lambda(Lambda(_),_) -> error context "cannot call lambda :-D" world
    
  | (HelpIJ _|HelpI _|AttackIJ _|AttackI _|Kx _ | ZombieI _ | Sfg _ | Sf _) as x -> 
    x,world
  | Error -> 
    error context "inter: badInsn" world


exception InterError of (string * default_world_type)
    
let create_default_world () = 
  let player0 = Array.make 256 (10000,Card(I)) in
  let player1 = Array.make 256 (10000,Card(I)) in
  player0,player1
    

let default_context = 
  let own = function a,_ -> a in
  let other = function _,a -> a in
  let vit = function vit,_ -> vit in
  let field = function _,field -> field in
  let get what which i (w:default_world_type) = (what (Array.get (which w) i)),w  in
  let rvit vit = function _,field -> vit,field in
  let rfield field = function vit,_ -> vit,field in
  let set what which i value (w:default_world_type) = 
    ((Array.set (which w) i (what value (Array.get (which w) i))); w) 
  in

  let count_alive which world = 
    let rec loop i acc = 
      let i = i + 1 in
      if i >= 256 then
	acc
      else
	let vit,_ = get vit which i world in
	if isalive vit then
	  loop i (acc + 1)
	else
	  loop i acc
    in
    (loop 0 0),world
  in

  { life = Alive;
    depth = 0;
    read_own_vit = (fun i w -> (get vit own i w));
    read_other_vit = (fun i w -> (get vit other i w));
    read_own_field = (fun i w -> (get field own i w));
    read_other_field = (fun i w -> (get field other i w));
    write_own_vit = (fun i vit w -> (set rvit own i vit w));
    write_other_vit = (fun i vit w -> (set rvit other i vit w));
    write_own_field = (fun i field w -> (set rfield own i field w));
    write_other_field = (fun i field w -> (set rfield other i field w));
    count_alive_own = (count_alive own);
    count_alive_other = (count_alive other);
    error = (fun msg w -> raise (InterError(msg,w)));
    end_move = (fun (p1,p2) (q1,q2) -> (p2,p1),(q2,q1));
    }
  
let movegetslot = function 
  | Left(_,slot) -> slot 
  | Right(slot,_) -> slot 
    
let apply_move move world context turn_stats debug = 
  let slot = movegetslot move in 
  let vit,world = context.read_own_vit slot world in
  try 
    if isalive vit then
      let field,world = context.read_own_field slot world in
      let expr = match move with 
	| Left(card,_) -> Lambda(Card(card),field)
	| Right(_,card) -> Lambda(field,Card(card))
      in let field,world = inter {context with life = Alive; depth = 0} world expr
      in
	context.write_own_field slot field world, turn_stats
    else
      (context.error "apply: not alive" world; (* assert false; *) world), turn_stats
  with InterError(msg,w) -> 
    debug (MsgReset slot);
    context.write_own_field slot (Card I) world, (
      { (fst turn_stats) with reset_slots = slot :: (fst turn_stats).reset_slots }, (snd turn_stats) 
    )

let apply_zombies world context = 
  let rec loop i world = 
    let vit,world = context.read_own_vit i world in
    let world = 
      if iszombie vit then
	let field,world = context.read_own_field i world in
	try 
	  let _,world = inter {context with life = Dead; depth = 0} world field in
	  world
	with InterError(msg,w) ->
	  w
      else
	world
    in
    if i = 255 then
      world
    else
      loop (i+1) world
  in loop 0 world

let apply_player context world turn_stats move debug =
  let world = apply_zombies world context in
  let world, turn_stats = apply_move move world context turn_stats debug in 
  let count,world = context.count_alive_own world in
  let world, turn_stats = context.end_move world turn_stats in
    count,world,move, turn_stats

let play_game context world player0_input player0_output_callback player1_input player1_output_callback printer startturn = 
  let winner world = 
    let count0 = context.count_alive_own world in
    let count1 = context.count_alive_other world in
    let winner = 
      if count0 > count1 then
	0 
      else if count1 > count0 then
	1
      else
	-1
    in
    winner,world
  in
  let rec loop i world = 
    if i >= 100000 then
      winner world
    else
      (printer (MsgTurn i);
       printer (MsgPlayer 0);
       printer (MsgWorld world);
       let move = player0_input () in
       printer (MsgMove (0, move));
       let count,world,move,_ = apply_player context world (empty_turn_stats ()) move printer in
       player1_output_callback move;
       if count = 0 then
	 1,world
       else
	 (printer (MsgPlayer 1);
	  printer (MsgWorld world);
	  let move = player1_input () in
	  printer (MsgMove (1, move));
	  let count,world,move,_ = apply_player context world (empty_turn_stats ()) move printer in
	  player0_output_callback move;
	  if count = 0 then 
	    0,world
	  else
	    loop (i+1) world))
  in
    loop startturn world

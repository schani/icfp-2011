open Cards

type msg_type =
  | MsgStartup
  | MsgTurn of int
  | MsgPlayer of int
  | MsgWorld of default_world_type
  | MsgQuestion
  | MsgMove of (int * turn)

(* generates a string "message" as specified in the task description *)
val msg_of_turn: turn -> string

(* generate pretty string of turn *)
val string_of_turn: turn -> string
  
(* world printer that prints like the demo tool *)
val std_world_printer: msg_type -> unit

val string_of_expr: skiexpr -> string

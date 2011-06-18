open Inter
open Parser
open Printer
open Arg
open Cards
open Bot

type privdata = { pd_turns : turn list }

let read_turns_from_file ifi printer =
  let l = ref []
  in
    try
      while true
      do
	l := (parse_input ifi printer ()) :: !l
      done;
      !l
    with
	End_of_file -> List.rev !l

let move_callback context world proponent_move privdata =
  let move, rest =
    match privdata.pd_turns with
      | [] -> Left (I, 18), []
      | turn :: rest -> turn, rest
  in
    move, { pd_turns = rest }

let _ =
  let ifi = open_in "/icfpnfs/SCHANI/killer-fn.cmd"
  and skipfirst = match Sys.argv with
    | [| _; "0" |] -> false
    | [| _; "1" |] -> true
    | _ -> failwith "dunno what to do, gimme some args"
  in let turns = read_turns_from_file ifi quiet_printer
  in let pd = { pd_turns = turns }
  in
    bootloop ~skipfirst move_callback pd

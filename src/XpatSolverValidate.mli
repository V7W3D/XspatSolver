exception Move_error;;
exception Invalid_format;;

type game = Freecell | Seahaven | Midnight | Baker

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type config = { mutable game : game; mutable seed: int; mutable mode: mode }

type state = {
  mutable columns : Card.card list FArray.t;
  mutable deposit : int FArray.t; 
  mutable registers : Card.card option FArray.t;
  mutable rank : int;
}

type destination = T | V | Id of int 

val set_game_seed : string -> unit
val get_config : unit -> config
val move : int -> destination -> state -> unit
val validate_file : in_channel-> state -> unit
val init_columns : (Card.card list list) -> state -> unit
val split_permut : (int list) -> state -> (Card.card list list)
val set_state : game -> state
val get_dst_ind : int -> state -> int
val normalize : state -> unit

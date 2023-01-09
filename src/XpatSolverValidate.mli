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
}

type destination = T | V | Id of int 

val set_game_seed : string -> unit
val get_config : unit -> config
val move : int -> destination -> unit
val validate_file : in_channel -> unit
val init_columns : (Card.card list list) -> unit
val split_permut : (int list) -> (Card.card list list)
val set_state : game -> unit
val set_state_s : state -> unit
val get_dst_ind : int -> int
val normalize : unit -> unit
val get_state : unit -> state
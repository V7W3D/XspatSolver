
open XpatLib

type game = Freecell | Seahaven | Midnight | Baker

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type config = { mutable game : game; mutable seed: int; mutable mode: mode }

let config = { game = Freecell; seed = 1; mode = Search "" }

type state = {
  mutable columns : Card.card list FArray.t;
  mutable deposit : int FArray.t; 
  mutable registers : Card.card option FArray.t;
}

let state = { columns = FArray.make 8 []; deposit = FArray.make 4 0; registers = FArray.make 4 None }

let getgame = function
  | "FreeCell"|"fc" -> Freecell
  | "Seahaven"|"st" -> Seahaven
  | "MidnightOil"|"mo" -> Midnight
  | "BakersDozen"|"bd" -> Baker
  | _ -> raise Not_found

let split_on_dot name =
  match String.split_on_char '.' name with
  | [string1;string2] -> (string1,string2)
  | _ -> raise Not_found

let set_game_seed name =
  try
    let (sname,snum) = split_on_dot name in
    config.game <- getgame sname;
    config.seed <- int_of_string snum
  with _ -> failwith ("Error: <game>.<number> expected, with <game> in "^
                      "FreeCell Seahaven MidnightOil BakersDozen")

let set_state = function
  | Seahaven -> state.columns <- FArray.make 10 []
  | Midnight -> state.columns <- FArray.make 18 []
  | Baker -> state.columns <- FArray.make 13 []
  | _ -> ()

let split_permut_fc permut = 
  let rec split_permut_fc_aux permut accll accl cpt altern = 
    match permut with 
    | [] -> accl::accll
    | x::l -> 
      if altern then 
        if cpt < 7 then split_permut_fc_aux (l) (accll) ((Card.of_num x)::accl) (cpt+1) (altern) 
        else split_permut_fc_aux (l) (accl::accll) ((Card.of_num x)::[]) (1) (not altern)
      else
        if cpt < 6 then split_permut_fc_aux (l) (accll) ((Card.of_num x)::accl) (cpt+1) (altern)
        else split_permut_fc_aux (l) (accl::accll) ((Card.of_num x)::[]) (1) (not altern)
  in split_permut_fc_aux (permut) ([]) ([]) (0) (true)

let split_permut_st permut = 
  let rec split_permut_st_aux permut accll accl cpt = 
    match permut with
    | [] -> accl::accll
    | [c1;c2] -> begin
      state.registers <- FArray.set (state.registers) (0) (Some (Card.of_num c1));
      state.registers <- FArray.set (state.registers) (1) (Some (Card.of_num c1));
      split_permut_st_aux [] accll accl cpt
    end
    | x::l -> if cpt < 5 then split_permut_st_aux (l) (accll) ((Card.of_num x)::accl) (cpt+1)
    else split_permut_st_aux (x::l) (accl::accll) ([]) (0)
  in split_permut_st_aux (permut) ([]) ([]) (0)

let split_permut_mo permut = 
  let rec split_permut_mo_aux permut accll accl cpt = 
    match permut with
    | [] -> accl::accll
    | x::l -> if cpt < 3 then split_permut_mo_aux (l) (accll) ((Card.of_num x)::accl) (cpt+1)
    else split_permut_mo_aux (l) (accl::accll) ((Card.of_num x)::[]) (1)
  in split_permut_mo_aux (permut) ([]) ([]) (0)

let split_permut_bk permut = 
  let rec split_permut_bk_aux permut accll accl cpt = 
    match permut with
    | [] -> accl::accll
    | x::l -> match Card.of_num x with
      | (13, _) -> if cpt < 4 then split_permut_bk_aux (l) (accll) (accl@((Card.of_num x)::[])) (cpt+1) else split_permut_bk_aux (l) (accl::accll) ((Card.of_num x)::[]) (1)
      | (_, _) -> if cpt < 4 then split_permut_bk_aux (l) (accll) ((Card.of_num x)::accl) (cpt+1) else split_permut_bk_aux (l) (accl::accll) ((Card.of_num x)::[]) (1)
  in split_permut_bk_aux (permut) ([]) ([]) (0)

let init_columns splited_permut = 
  let rec init_columns_aux splited_permut cpt = match splited_permut with
    | [] -> ()
    | x :: l -> begin
      state.columns <- FArray.set (state.columns) (cpt) (x);
      init_columns_aux (l) (cpt+1)
      end
  in init_columns_aux (splited_permut) (0)

let rec affiche_colonnes ll = 
  match ll with
   | [] -> ()
   | x::l -> 
    begin 
      List.iter (fun n -> Printf.printf "%s " (Card.to_string (n))) x; 
      print_newline ();
      affiche_colonnes l
    end

let split_permut permut = match config.game with
  | Freecell -> split_permut_fc permut
  | Seahaven -> split_permut_st permut
  | Midnight -> split_permut_mo permut
  | Baker -> split_permut_bk permut
(* TODO : La fonction suivante est à adapter et continuer *)

let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  Printf.printf "Voici juste la permutation de graine %d:\n" conf.seed;
  List.iter (fun n -> print_int n; print_string " ") permut;
  print_newline ();
  List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n)))
    permut;
  print_newline ();
  (*print_string "C'est tout pour l'instant. TODO: continuer...\n";*)
  set_state conf.game;
  print_newline ();
  let res = split_permut permut in affiche_colonnes res;
  init_columns res;
  exit 0

let main () =
  Arg.parse
    [("-check", String (fun filename -> config.mode <- Check filename),
        "<filename>:\tValidate a solution file");
     ("-search", String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file")]
    set_game_seed (* pour les arguments seuls, sans option devant *)
    "XpatSolver <game>.<number> : search solution for Xpat2 game <number>";
  treat_game config

let _ = if not !Sys.interactive then main () else ()

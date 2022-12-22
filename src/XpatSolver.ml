
open XpatLib

exception Move_error;;
exception Invalid_format;;

type game = Freecell | Seahaven | Midnight | Baker

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type config = { mutable game : game; mutable seed: int; mutable mode: mode }

let config = { game = Baker; seed = 1; mode = Search "" }

type state = {
  mutable columns : Card.card list FArray.t;
  mutable deposit : int FArray.t; 
  mutable registers : Card.card option FArray.t;
}

let state = { columns = FArray.make 8 []; deposit = FArray.make 4 0; registers = FArray.make 4 None }

type destination = T | V | Id of int 

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

let rec put_in_deposit column index = match column with 
  | [] -> ()
  | x::l -> match x with
    | (rank, suit) -> begin
      if rank = (FArray.get (state.deposit) (Card.num_of_suit suit)) + 1 then
        begin
          state.deposit <- FArray.set (state.deposit) (Card.num_of_suit suit) (rank + 1);
          put_in_deposit (l) (index);
          state.columns <- FArray.set (state.columns) (index) (l)
        end
    end

let normalize () = 
  let rec normalize_aux index = match FArray.get (state.columns) (index) with
    | exception Not_found -> ()
    | c -> begin
      put_in_deposit (c) (index);
      normalize_aux (index + 1)
    end
  in normalize_aux (0)

let get_src_ind s = 
  let rec get_src_ind_aux s index = match FArray.get (state.columns) (index) with
    | [] -> get_src_ind_aux (s) (index + 1) 
    | c -> match List.hd c with
      | x -> if (Card.to_num x) = s then index else get_src_ind_aux (s) (index + 1) 
  in get_src_ind_aux s (0)

let get_dst_ind d =
  let rec get_dst_ind_aux d index = match FArray.get (state.columns) (index) with
  | [] -> get_dst_ind_aux (d) (index + 1)
  | c -> match List.hd c with
    | x -> if (Card.to_num x) = d then index else get_dst_ind_aux (d) (index + 1)
  in get_dst_ind_aux d (0)

let altern_color c1 c2 = match c1 with (_, x) -> match c2 with (_, y) -> (Card.num_of_suit x) - (Card.num_of_suit y) > 1 

let inferior_rank c1 c2 = match c1 with (x, _) -> match c2 with (y, _) -> if x < y then true else false

let first_empty_register () =
  let rec first_empty_register_aux index = match FArray.get (state.registers) (index) with
    | Some x -> first_empty_register_aux (index + 1)
    | None -> index
  in first_empty_register_aux (0)

let first_empty_column () = 
  let rec first_empty_column_aux index = match FArray.get (state.columns) (index) with
    | [] -> index
    |  l -> first_empty_column_aux (index + 1)
  in first_empty_column_aux (0)

let move_fc s d = match d with
  | Id x -> (match get_src_ind s with
    | exception Not_found -> raise Move_error
    | i -> match get_dst_ind x with
      | exception Not_found -> raise Move_error
      | j -> (match FArray.get (state.columns) (i) with
        | c1 :: l1 -> (match FArray.get (state.columns) (j) with
          | c2 :: l2 -> 
            if (inferior_rank c1 c2) && (altern_color c1 c2) then
            begin
              state.columns <- FArray.set (state.columns) (i) (l1);
              state.columns <- FArray.set (state.columns) (j)  (c1::(c2::l2))
            end
            else raise Move_error
          | _ -> ())
        |_ -> ()))
  | T -> (match get_src_ind s with
    | exception Not_found -> raise Move_error
    | i -> match FArray.get (state.columns) (i) with
      | c :: l1 -> (match first_empty_register () with
        | exception Not_found -> raise Move_error
        | x -> state.registers <- FArray.set (state.registers) (x) (Some c))
      | _ -> ())
  | V -> (match get_src_ind s with
    | exception Not_found -> raise Move_error
    | i -> match FArray.get (state.columns) (i) with
      | c :: l1 -> (match first_empty_column () with
        | exception Not_found -> raise Move_error
        | x -> state.registers <- FArray.set (state.registers) (x) (Some c))
      | _ -> ())

let same_suit c1 c2 =  match c1 with (_, x) -> match c2 with (_, y) -> (Card.num_of_suit x) = (Card.num_of_suit y)

let move_st s d = match d with
  | Id x -> (match get_src_ind s with
    | exception Not_found -> raise Move_error
    | i -> match get_dst_ind x with
      | exception Not_found -> raise Move_error
      | j -> (match FArray.get (state.columns) (i) with
        | c1 :: l1 -> (match FArray.get (state.columns) (j) with
          | c2 :: l2 -> 
            if (inferior_rank c1 c2) && (same_suit c1 c2) then
            begin
              state.columns <- FArray.set (state.columns) (i) (l1);
              state.columns <- FArray.set (state.columns) (j)  (c1::(c2::l2))
            end
            else raise Move_error
          | _ -> ())
        |_ -> ()))
  | T -> (match get_src_ind s with
    | exception Not_found -> raise Move_error
    | i -> match FArray.get (state.columns) (i) with
      | c :: l1 -> (match first_empty_register () with
        | exception Not_found -> raise Move_error
        | x -> state.registers <- FArray.set (state.registers) (x) (Some c))
      | _ -> ())
  | V -> (match get_src_ind s with
    | exception Not_found -> raise Move_error
    | i -> (match FArray.get (state.columns) (i) with
      | c :: l1 -> (match c with (x, _) -> if x = 13 then 
        (match first_empty_column () with
          | exception Not_found -> raise Move_error
          | x -> state.registers <- FArray.set (state.registers) (x) (Some c))
        else raise Move_error)
      | _ -> ()))

let move_mo s d = match d with
  | Id x -> (match get_src_ind s with
    | exception Not_found -> raise Move_error
    | i -> match get_dst_ind x with
      | exception Not_found -> raise Move_error
      | j -> (match FArray.get (state.columns) (i) with
        | c1 :: l1 -> (match FArray.get (state.columns) (j) with
          | c2 :: l2 -> 
            if (inferior_rank c1 c2) && (same_suit c1 c2) then
            begin
              state.columns <- FArray.set (state.columns) (i) (l1);
              state.columns <- FArray.set (state.columns) (j)  (c1::(c2::l2))
            end
            else raise Move_error
          | _ -> ())
        |_ -> ()))
  | T -> ()
  | V -> ()

let move_bk s d = match d with
  | Id x -> (match get_src_ind s with
    | exception Not_found -> raise Move_error
    | i -> match get_dst_ind x with
      | exception Not_found -> raise Move_error
      | j -> (match FArray.get (state.columns) (i) with
        | c1 :: l1 -> (match FArray.get (state.columns) (j) with
          | c2 :: l2 -> 
            if (inferior_rank c1 c2) then
            begin
              state.columns <- FArray.set (state.columns) (i) (l1);
              state.columns <- FArray.set (state.columns) (j)  (c1::(c2::l2))
            end
            else raise Move_error
          | _ -> ())
        |_ -> ()))
  | T -> ()
  | V -> ()

let move s d = match config.game with
| Freecell -> move_fc s d
| Seahaven -> move_st s d
| Midnight -> move_mo s d
| Baker -> move_bk s d

let validate_file f = 
  let rec validate_file_aux f n = (match input_line f with
    | line -> let l = Str.split (Str.regexp " ") (line) in (match l with
      | [i; j] -> (match j with
        | "T" -> (try move (int_of_string i) (T); normalize (); validate_file_aux (f) (n+1) with Move_error -> Printf.printf "ECHEC %d\n" n; exit 1) 
        | "V" -> (try move (int_of_string i) (V); normalize (); validate_file_aux (f) (n+1) with Move_error -> Printf.printf "ECHEC %d\n" n; exit 1)
        | _ -> (try move (int_of_string i) (Id (int_of_string j)); normalize (); validate_file_aux (f) (n+1) with Move_error -> Printf.printf "ECHEC %d\n" n; exit 1))
      | _ -> raise Invalid_format)
    | exception End_of_file -> Printf.printf "SUCCESS\n"; close_in f)
  in validate_file_aux f 0
    
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
  let res = split_permut permut in init_columns res;
  normalize ();
  match config.mode with
    | Check filename -> validate_file (open_in filename)
    | _ -> ();
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

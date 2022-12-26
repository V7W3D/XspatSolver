
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

let rec print_columns index = match FArray.get (state.columns) (index) with
  | exception Not_found -> ()
  | c -> begin 
    List.iter (fun n -> Printf.printf "%s " (Card.to_string n)) c; 
    print_newline ();
    print_columns (index + 1)
end

let rec print_registers index = match FArray.get (state.registers) (index) with
  | exception Not_found -> ()
  | None -> Printf.printf "N "; print_registers (index + 1)
  | Some c -> Printf.printf "%s " (Card.to_string c); print_registers (index + 1)

let rec print_deposit index = match FArray.get (state.deposit) (index) with
  | exception Not_found -> ()
  | c -> Printf.printf "%d " c; print_deposit (index + 1)

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

let split_permut permut = match config.game with
  | Freecell -> split_permut_fc permut
  | Seahaven -> split_permut_st permut
  | Midnight -> split_permut_mo permut
  | Baker -> split_permut_bk permut

let rec put_in_deposit_col column index check = match column with 
  | [] -> check
  | x::l -> match x with
    | (rank, suit) -> begin
      if rank = ((FArray.get (state.deposit) (Card.num_of_suit suit)) + 1) then
        begin
          state.deposit <- FArray.set (state.deposit) (Card.num_of_suit suit) (rank);
          state.columns <- FArray.set (state.columns) (index) (l);
          put_in_deposit_col (l) (index) (true)
        end
      else check
    end

let rec normalize_reg index check = match FArray.get (state.registers) (index) with
  | exception Not_found -> check
  | Some c -> (match c with (rank, suit) -> 
    if (rank = (FArray.get (state.deposit) (Card.num_of_suit suit)) + 1) then
      begin
        state.deposit <- FArray.set (state.deposit) (Card.num_of_suit suit) (rank);
        state.registers <- FArray.set (state.registers) (index) (None);
        normalize_reg (index + 1) (true)
      end
    else normalize_reg (index + 1) (check))
  | None -> normalize_reg (index + 1) (check)

let rec normalize_col index check = match FArray.get (state.columns) (index) with
  | exception Not_found -> check
  | c -> let check_updated = put_in_deposit_col (c) (index) (check) in normalize_col (index + 1) (check_updated)

let rec normalize () = let check1 = normalize_col (0) (false) in let check2 = normalize_reg (0) (false) in if (check1 || check2) then (print_string "fvck"; normalize ())

let get_src_col_ind s = 
  let rec get_src_col_ind_aux s index = match FArray.get (state.columns) (index) with
    | [] -> get_src_col_ind_aux (s) (index + 1) 
    | c -> match List.hd c with
      | x -> if (Card.to_num x) = s then index else get_src_col_ind_aux (s) (index + 1) 
  in get_src_col_ind_aux s (0)

let get_src_reg_ind s = 
  let rec get_src_reg_ind_aux s index = match FArray.get (state.registers) (index) with
  | None -> get_src_reg_ind_aux (s) (index + 1) 
  | Some c -> if (Card.to_num c) = s then index else get_src_reg_ind_aux (s) (index + 1) 
in get_src_reg_ind_aux s (0)

let get_dst_ind d =
  let rec get_dst_ind_aux d index = match FArray.get (state.columns) (index) with
    | [] -> get_dst_ind_aux (d) (index + 1)
    | c -> match List.hd c with
      | x -> if (Card.to_num x) = d then index else get_dst_ind_aux (d) (index + 1)
  in get_dst_ind_aux d (0)

let altern_color c1 c2 = match c1 with (_, x) -> match c2 with (_, y) -> ((Card.num_of_suit x) > 1 && (Card.num_of_suit y) < 2) || ((Card.num_of_suit y) > 1  && (Card.num_of_suit x) < 2)

let inferior_rank c1 c2 = match c1 with (x, _) -> match c2 with (y, _) -> y = (x + 1)

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
  | Id x -> (match get_src_col_ind s with
    | exception Not_found -> (match get_src_reg_ind s with
      | exception Not_found -> raise Move_error
      | i -> match get_dst_ind x with
        | exception Not_found -> raise Move_error
        | j -> (match FArray.get (state.registers) (i) with
          | Some c1 -> (match FArray.get (state.columns) (j) with
            | c2 :: l2 -> 
              if (inferior_rank c1 c2) && (altern_color c1 c2) then
              begin
                state.registers <- FArray.set (state.registers) (i) (None);
                state.columns <- FArray.set (state.columns) (j)  (c1::(c2::l2));
              end
              else raise Move_error
            | _ -> ())
          |_ -> ()))
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
  | T -> (match get_src_col_ind s with
    | exception Not_found -> raise Move_error
    | i -> match FArray.get (state.columns) (i) with
      | c :: l1 -> (match first_empty_register () with
        | exception Not_found -> raise Move_error
        | x -> begin 
          state.registers <- FArray.set (state.registers) (x) (Some c);
          state.columns <- FArray.set (state.columns) (i) (l1)
        end)
      | _ -> ())
  | V -> (match get_src_col_ind s with
    | exception Not_found -> raise Move_error
    | i -> match FArray.get (state.columns) (i) with
      | c :: l1 -> (match first_empty_column () with
        | exception Not_found -> raise Move_error
        | x -> begin 
          state.columns <- FArray.set (state.columns) (x) ([c]);
          state.columns <- FArray.set (state.columns) (i) (l1)
          end)
      | _ -> ())

let same_suit c1 c2 =  match c1 with (_, x) -> match c2 with (_, y) -> (Card.num_of_suit x) = (Card.num_of_suit y)

let move_st s d = match d with
  | Id x -> (match get_src_col_ind s with
    | exception Not_found -> (match get_src_reg_ind s with
      | exception Not_found -> raise Move_error
      | i -> match get_dst_ind x with
        | exception Not_found -> raise Move_error
        | j -> (match FArray.get (state.registers) (i) with
          | Some c1 -> (match FArray.get (state.columns) (j) with
            | c2 :: l2 -> 
              if (inferior_rank c1 c2) && (same_suit c1 c2) then
              begin
                state.registers <- FArray.set (state.registers) (i) (None);
                state.columns <- FArray.set (state.columns) (j)  (c1::(c2::l2))
              end
              else raise Move_error
            | _ -> ())
          |_ -> ()))
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
  | T -> (match get_src_col_ind s with
    | exception Not_found -> raise Move_error
    | i -> match FArray.get (state.columns) (i) with
      | c :: l1 -> (match first_empty_register () with
        | exception Not_found -> raise Move_error
        | x -> begin 
          state.registers <- FArray.set (state.registers) (x) (Some c);
          state.columns <- FArray.set (state.columns) (i) (l1)
          end)
      | _ -> ())
  | V -> (match get_src_col_ind s with
    | exception Not_found -> (match get_src_reg_ind s with
      | exception Not_found -> raise Move_error
      | i -> (match FArray.get (state.registers) (i) with
        | Some c -> (match c with (x, _) -> if x = 13 then 
          (match first_empty_column () with
          | exception Not_found -> raise Move_error
          | j -> begin
            state.registers <- FArray.set (state.registers) (i) (None);
            state.columns <- FArray.set (state.columns) (j) ([c])
            end)
          else raise Move_error)
        | _ -> ()))
    | i -> (match FArray.get (state.columns) (i) with
      | c :: l1 -> (match c with (x, _) -> if x = 13 then 
        (match first_empty_column () with
        | exception Not_found -> raise Move_error
        | j -> begin 
          state.columns <- FArray.set (state.columns) (j) ([c]);
          state.columns <- FArray.set (state.columns) (i) (l1)
          end)
        else raise Move_error)
      | _ -> ()))

let move_mo s d = match d with
  | Id x -> (match get_src_col_ind s with
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
  | Id x -> (match get_src_col_ind s with
    | exception Not_found -> raise Move_error
    | i -> match get_dst_ind x with
      | exception Not_found -> raise Move_error
      | j -> (match FArray.get (state.columns) (i) with
        | c1 :: l1 -> (match FArray.get (state.columns) (j) with
          | c2 :: l2 -> 
            if (inferior_rank c1 c2) then
            begin
              state.columns <- FArray.set (state.columns) (i) (l1);
              state.columns <- FArray.set (state.columns) (j) (c1::(c2::l2))
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

let validate_deposit () = 
  let rec validate_deposit_aux index = match FArray.get (state.deposit) (index) with
  | exception Not_found -> true
  | c -> if c = 13 then validate_deposit_aux (index + 1) else false
  in validate_deposit_aux (0)


let validate_file f = 
  let rec validate_file_aux f n = (match input_line f with
    | line -> let l = String.split_on_char (' ') (line) in (match l with
      | [i; j] -> (match j with
        | "T" -> (try move (int_of_string i) (T); normalize (); validate_file_aux (f) (n+1) with Move_error ->print_deposit (0); print_newline (); print_registers (0); print_newline (); print_columns (0); print_newline (); Printf.printf "ECHEC %d\n" n; exit 1) 
        | "V" -> (try move (int_of_string i) (V); normalize (); validate_file_aux (f) (n+1) with Move_error -> print_registers (0); print_newline (); print_columns (0); print_newline (); Printf.printf "ECHEC %d\n" n; exit 1)
        | _ -> (try move (int_of_string i) (Id (int_of_string j)); normalize (); validate_file_aux (f) (n+1) with Move_error -> print_columns (0); print_newline (); Printf.printf "ECHEC %d\n" n; exit 1))
      | _ -> raise Invalid_format)
    | exception End_of_file -> (match validate_deposit () with
      | false -> (print_columns (0); print_newline (); Printf.printf "ECHEC %d\n" n)
      | true -> Printf.printf "SUCESS\n"))
  in validate_file_aux (f) (1)

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
  print_columns (0); print_newline ();
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

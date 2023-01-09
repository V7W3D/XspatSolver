open XpatLib

let treat_game (conf:XpatSolverValidate.config) : unit =
  let permut = XpatRandom.shuffle conf.seed in
  XpatSolverValidate.set_state conf.game;
  let res = XpatSolverValidate.split_permut permut 
      in XpatSolverValidate.init_columns res;
  XpatSolverValidate.normalize ();
  match conf.mode with
    | Check filename -> XpatSolverValidate.validate_file (open_in filename)
    | Search filename -> if (XpatSolverSearch.search ()) then () else ();
  exit 0

let main () =
  let config = XpatSolverValidate.get_config () in
  Arg.parse
    [("-check", String (fun filename -> config.mode <- Check filename),
        "<filename>:\tValidate a solution file");
     ("-search", String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file")
     ]
    XpatSolverValidate.set_game_seed (* pour les arguments seuls, sans option devant *)
    "XpatSolver <game>.<number> : search solution for Xpat2 game <number>";
  treat_game config

let _ = if not !Sys.interactive then main () else ()

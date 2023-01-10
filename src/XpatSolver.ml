open XpatLib

let treat_game (conf:XpatSolverValidate.config) : unit =
  let permut = XpatRandom.shuffle conf.seed in
  let state = XpatSolverValidate.set_state conf.game in
  let res = XpatSolverValidate.split_permut permut state
      in XpatSolverValidate.init_columns res state;
  XpatSolverValidate.normalize state;
    match conf.mode with
      | Check filename -> XpatSolverValidate.validate_file (open_in filename) (state)
      | Search filename -> XpatSolverSearch.search filename state

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

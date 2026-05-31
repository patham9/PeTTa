:- ensure_loaded(metta).

prologfunc(X,Y) :- Y is X+1.

prolog_interop_example :- register_fun(prologfunc),
                          process_metta_string("(= (mettafunc $x) (prologfunc $x))", _),
                          listing(mettafunc),
                          mettafunc(30, R),
                          format("mettafunc(30) = ~w~n", [R]).

main :- current_prolog_flag(argv, Args),
        catch(run_main(Args),
              debug_break_abort,
              format(user_error, "~n[debugger] aborted by user~n", [])),
        halt.

run_main(Args) :-
        ( debug_help_requested(Args) -> print_debug_help
        ; file_argument(Args, File) -> file_directory_name(File, Dir),
                                      assertz(working_dir(Dir)),
                                      load_metta_file(File,Results),
                                      maplist(swrite,Results,ResultsR),
                                      maplist(format("~w~n"), ResultsR)
        ; mork_enabled(Args) -> prolog_interop_example,
                                mork_test
        ; prolog_interop_example
        ).

:- initialization(main, main).

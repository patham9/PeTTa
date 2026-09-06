:- ensure_loaded(metta).

is_silent_flag(silent).
is_silent_flag('--silent').
is_silent_flag('-s').

strip_silent_flags([], []).
strip_silent_flags([Arg|Rest], Filtered) :-
        is_silent_flag(Arg),
        !,
        strip_silent_flags(Rest, Filtered).
strip_silent_flags([Arg|Rest], [Arg|Filtered]) :-
        strip_silent_flags(Rest, Filtered).

prologfunc(X,Y) :- Y is X+1.

prolog_interop_example :- register_fun(prologfunc),
                          process_metta_string("(= (mettafunc $x) (prologfunc $x))", _),
                          listing(mettafunc),
                          mettafunc(30, R),
                          format("mettafunc(30) = ~w~n", [R]).

main :- current_prolog_flag(argv, RawArgs),
        strip_silent_flags(RawArgs, Args),
        catch(run_main(Args),
              debug_break_abort,
              format(user_error, "~n[debugger] aborted by user~n", [])),
        halt.

run_main(Args) :-
        ( debug_help_requested(Args) -> print_debug_help
        ; Args == [mork] -> prolog_interop_example,
                            mork_test
        ; file_argument(Args, File) -> run_metta_file(File)
        ; debug_args_present(Args) -> usage_error("no .metta file given")
        ; mork_enabled(Args) -> prolog_interop_example,
                                mork_test
        ; prolog_interop_example
        ).

run_metta_file(File) :-
        ( exists_file(File)
          -> file_directory_name(File, Dir),
             assertz(working_dir(Dir)),
             load_metta_file(File,Results),
             maplist(swrite,Results,ResultsR),
             maplist(format("~w~n"), ResultsR)
        ; format(atom(Msg), "file not found: ~w", [File]),
          usage_error(Msg)
        ).

usage_error(Reason) :-
        format(user_error, "PeTTa: ~w~n", [Reason]),
        format(user_error, "Usage: sh debug.sh <file.metta> [options]   (run 'sh debug.sh --debug-help')~n", []),
        halt(2).

:- initialization(main, main).

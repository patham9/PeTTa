:- ensure_loaded(metta).

prologfunc(X,Y) :- Y is X+1.

prolog_interop_example :- metta:import_user_fun(prologfunc),
                          metta:register_fun(prologfunc),
                          metta:process_metta_string("(= (mettafunc $x) (prologfunc $x))", _),
                          listing(metta:mettafunc/2),
                          metta:mettafunc(30, R),
                          format("mettafunc(30) = ~w~n", [R]).

main :- current_prolog_flag(argv, Args),
        ( Args = [] -> prolog_interop_example
        ; Args = [mork] -> prolog_interop_example,
                           metta:mork_test
        ; Args = [File|_] -> file_directory_name(File, Dir),
                             assertz(metta:working_dir(Dir)),
                             metta:load_metta_file(File,Results),
                             maplist(metta:swrite,Results,ResultsR),
                             maplist(format("~w~n"), ResultsR)
        ),
        halt.

:- initialization(main, main).

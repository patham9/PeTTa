:- ensure_loaded(metta).

prologfunc(X,Y) :- Y is X+1.

prolog_interop_example :- register_fun(prologfunc),
                          assert_function("(= (mettafunc $x) (prologfunc $x))"),
                          listing(mettafunc),
                          mettafunc(30, R),
                          format("mettafunc(30) = ~w~n", [R]).

main :- current_prolog_flag(argv, Args),
        ( Args = [] -> prolog_interop_example
        ; Args = [mork] -> prolog_interop_example,
                           mork_test
        ; Args = [File|_] -> file_directory_name(File, Dir),
                             assertz(working_dir(Dir)),
                             load_metta_file(File,default) ),
        halt.

:- initialization(main, main).

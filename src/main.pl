:- ensure_loaded(metta).

prologfunc(X,Y) :- Y is X+1.

prolog_interop_example :- register_fun(prologfunc),
                          assert_function("(= (mettafunc $x) (prologfunc $x))"),
                          listing(mettafunc),
                          mettafunc(30, R),
                          format("mettafunc(30) = ~w~n", [R]).


ends_with(Atom, Suffix) :- sub_atom(Atom, _, _, 0, Suffix).
halt_gracefully(Msg, Code):- format("~w~n", [Msg]),
                             halt(Code).


main :- current_prolog_flag(argv, Args),
        ( Args = [] -> prolog_interop_example
                     ; ((member(Arg, Args), ends_with(Arg, '.metta')) ->  File=Arg; halt_gracefully("No '.metta' file provided!", 1)),
                       file_directory_name(File, Dir),
                       assertz(working_dir(Dir)),
                       load_metta_file(File,default),
                       findall(R, run(default,R), Results),
                       maplist(swrite, Results, Strings),
                       format("~w~n", [Strings]) ),
        halt.

:- initialization(main, main).

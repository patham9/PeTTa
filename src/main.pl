:- ensure_loaded(metta).

prologfunc(X,Y) :- Y is X+1.

prolog_interop_example :- register_fun(prologfunc),
                          assert_function("(= (mettafunc $x) (prologfunc $x))"),
                          listing(mettafunc),
                          mettafunc(30, R),
                          format("mettafunc(30) = ~w~n", [R]).

main :- mork_init,
        current_prolog_flag(argv, Args),
        ( Args = [] -> prolog_interop_example,
                       mork_test
                     ; Args = [File|_],
                       load_metta_file(File,default),
                       findall(R, run(default,R), Results),
                       maplist(swrite, Results, Strings),
                       format("~w~n", [Strings]) ),
        halt.

:- initialization(main, main).

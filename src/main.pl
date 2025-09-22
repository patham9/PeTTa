:- ensure_loaded(metta).

prologfunc(X,Y) :- Y is X+1.

prolog_interop_example :- register_fun(prologfunc),
                          assert_function("(= (mettafunc $x) (prologfunc $x))"),
                          compile_metta_code,
                          listing(mettafunc),
                          mettafunc(30, R),
                          format("mettafunc(30) = ~w~n", [R]).

main :- current_prolog_flag(argv, Args),
        ( Args = [] -> prolog_interop_example
                     ; Args = [File|_],
                       load_metta_file(File),
                       compile_metta_code,
                       findall(R, run(R), Results),
                       format("~w~n", [Results]) ).

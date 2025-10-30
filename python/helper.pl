process_metta_string_silent(S, ResultsR) :-
    ( current_prolog_flag(argv, Args) -> true ; Args = [] ),
    append(Args, ['--silent'], Args1),
    set_prolog_flag(argv, Args1),
    process_metta_string(S, Results),
    repr(Results, ResultsR).

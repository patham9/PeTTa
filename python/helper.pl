:- ensure_loaded('../src/metta').

maybe_enable_silent(true) :- !.
maybe_enable_silent(false) :-
    ( current_prolog_flag(argv, Args0) -> true ; Args0 = [] ),
    ( member('--silent', Args0) -> Args1 = Args0
                                 ; append(Args0, ['--silent'], Args1)),
    set_prolog_flag(argv, Args1).

set_working_dir(load_metta_file, File) :-
    file_directory_name(File, Dir),
    retractall(metta:working_dir(_)),
    assertz(metta:working_dir(Dir)),
    !.
set_working_dir(_, _).

run_metta_helper(Verbose, Predicate, Arg, ResultsR) :-
    maybe_enable_silent(Verbose),
    set_working_dir(Predicate, Arg),
    Goal =.. [Predicate, Arg, Results],
    call(metta:Goal),
    maplist(metta:swrite,Results,ResultsR).

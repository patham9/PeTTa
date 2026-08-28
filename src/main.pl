% Default all stream I/O to UTF-8.
%
% MeTTa source files are UTF-8 by convention, but SWI-Prolog derives its
% default `encoding` flag from the ambient locale. Where that locale is not
% UTF-8 - Windows (cp1252), or any POSIX/C locale, which minimal container
% images and CI runners commonly have - read_file_to_string/3 in
% filereader.pl decodes .metta sources in the locale codepage instead, and
% multi-byte characters in string literals or symbol names silently become
% several wrong characters.
%
% Setting the flag here, before anything opens a stream, makes source loading
% deterministic regardless of environment. It is a no-op where the locale is
% already UTF-8.
:- set_prolog_flag(encoding, utf8).

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
        ( Args = [] -> prolog_interop_example
        ; Args = [mork] -> prolog_interop_example,
                           mork_test
        ; Args = [File|_] -> file_directory_name(File, Dir),
                             assertz(working_dir(Dir)),
                             load_metta_file(File,Results),
                             maplist(swrite,Results,ResultsR),
                             maplist(format("~w~n"), ResultsR)
        ),
        halt.

:- initialization(main, main).

:- multifile match/4.
:- multifile 'add-atom'/3.
:- multifile 'remove-atom'/3.
:- multifile 'get-atoms'/2.

%Add an atom to the space:
'add-atom'('&mork', Atom, true) :- !, swrite(Atom, S),
                                      mork("add-atoms", S, _).

%Remove all same atoms:
'remove-atom'('&mork', Atom, true) :- !, swrite(Atom, S),
                                         mork("remove-atoms", S, _).

%Match for pattern:
match('&mork', Pattern, OutPattern, Result) :- !, Pattern_Template = [Pattern, OutPattern],
                                                  swrite(Pattern_Template, MorkPat),
                                                  mork("match", MorkPat, Temp),
                                                  split_string(Temp, "\n", "", Raw),
                                                  exclude(==(""), Raw, Lines),
                                                  maplist(sread, Lines, Values),
                                                  member(Result, Values).

%Get all atoms in space, irregard of arity:
'get-atoms'('&mork', Pattern) :- !, mork("get-atoms", "", Temp),
                                    split_string(Temp, "\n", "", Raw),
                                    exclude(==(""), Raw, Lines),
                                    maplist(sread, Lines, Values),
                                    member(Pattern, Values).

%Execute MM2 calculus
'mm2-exec'('&mork', Steps, true) :- number_string(Steps, St),
                                    mork("mm2-exec", St, _).

%Init MORK:
:- working_directory(Cwd, Cwd),
   atomic_list_concat([Cwd, '/mork_ffi/morklib.so'], LibPath),
   use_foreign_library(LibPath),
   current_predicate(mork/3)
   -> writeln("MORK init: done")
    ; writeln("MORK init: failed").

%Test MORK:
mork_test :- 'add-atom'('&mork', [friend,sam,tim], true),
             'add-atom'('&mork', [friend,sam,joe], true),
             findall(C, match('&mork',[friend,sam,X], [friend,sam,X], C), Cs),
             format(string(SC), "MORK query result: ~w ~n", [Cs]), writeln(SC).

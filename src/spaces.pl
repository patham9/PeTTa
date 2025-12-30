%Since both normal add-attom call and function additions needs to add the S-expression:
add_sexp(Space, [Rel|Args]) :- Term =.. [Space, Rel | Args],
                               assertz(Term).

%Same but for removal:
remove_sexp(Space, [Rel|Args]) :- Term =.. [Space, Rel | Args],
                                  retractall(Term).

%Add a function atom:
'add-atom'(Space, Term, true) :- Term = [=,[FAtom|W],_], !,
                                 add_sexp(Space, Term),
                                 register_fun(FAtom),
                                 length(W, N),
                                 Arity is N + 1,
                                 assertz(arity(FAtom,Arity)),
                                 once(translate_clause(Term, Clause)),
                                 assertz(Clause),
                                 ( silent(true) -> true ; format("\e[33m--> added clause -->~n\e[32m", []),
                                                          Clause = (CHead :- CBody),
                                                          ( CBody == true -> Show = CHead; Show = (CHead :- CBody) ),
                                                          portray_clause(current_output, Show),
                                                          format("\e[33m^^^^^^^^^^^^^^^^^^^^^~n\e[0m") ).

%Add an atom to the space:
'add-atom'(Space, Term, true) :- add_sexp(Space, Term).

%%Remove a function atom:
'remove-atom'('&self', Term, Removed) :- Term = [=,[F|Ins],_], !,
                                         remove_sexp('&self', Term),
                                         once(translate_clause(Term, Cl)),
                                         ( retract(Cl) -> length(Ins, K),
                                                          unregister_fun(F/K),
                                                          Removed=true
                                                        ; Removed=false ).

%Remove all same atoms:
'remove-atom'(Space, Term, true) :- remove_sexp(Space, Term).

%Match for conjunctive pattern
match(_, LComma, OutPattern, Result) :- LComma == [','], !,
                                        Result = OutPattern.
match(Space, [Comma|[Head|Tail]], OutPattern, Result) :- Comma == ',', !,
                                                         append([Space], Head, List),
                                                         Term =.. List,
                                                         catch(Term, _, fail),
                                                         \+ cyclic_term(OutPattern),
                                                         match(Space, [','|Tail], OutPattern, Result).

% When the pattern list itself is a variable -> enumerate all atoms
match(Space, PatternVar, OutPattern, Result) :- var(PatternVar), !,
                                                'get-atoms'(Space, PatternVar),
                                                \+ cyclic_term(OutPattern),
                                                Result = OutPattern.

%Match for pattern:
match(Space, [Rel|PatArgs], OutPattern, Result) :- Term =.. [Space, Rel | PatArgs],
                                                   catch(Term, _, fail),
                                                   \+ cyclic_term(OutPattern),
                                                   Result = OutPattern.

%Get all atoms in space, irregard of arity:
'get-atoms'(Space, Pattern) :- current_predicate(Space/Arity),
                               functor(Head, Space, Arity),
                               clause(Head, true),
                               Head =.. [Space | Pattern].

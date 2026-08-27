%Since both normal add-attom call and function additions needs to add the S-expression:
add_sexp(Space, [Rel|Args]) :- Term =.. [Space, Rel | Args],
                               assertz(Term).

%Same but for removal:
remove_sexp(Space, [Rel|Args]) :- Term =.. [Space, Rel | Args],
                                  retractall(Term).

%Arrow declarations alter the code emitted for stored functions.
'add-atom'(Space, Term, true) :- atom(Space),
                                 function_type_annotation_term(Term, F, TypeChain), !,
                                 ( predeclared_function_type_variant(Space, F, TypeChain)
                                   -> add_sexp(Space, Term)
                                   ; revise_function_type(Space, F, add_sexp(Space, Term)) ).

%Add a function atom to its owner's compiled program:
'add-atom'(Space, Term, true) :- Term = [=,[FAtom|W],_], atom(Space), atom(FAtom), !,
                                 add_sexp(Space, Term),
                                 register_space_fun(Space, FAtom, Pred),
                                 length(W, N),
                                 Arity is N + 1,
                                 assertz(arity(Pred,Arity)),
                                 install_function_clause(Space, Term, Ref),
                                 metta_on_function_changed(Pred),
                                 invalidate_specializations(Pred),
                                 ( silent(true) -> true
                                 ; clause(Head, Body, Ref),
                                   maybe_print_compiled_clause("added function", Term, (Head :- Body)) ).

%Add an atom to the space:
'add-atom'(Space, Term, true) :- add_sexp(Space, Term).

'remove-atom'(Space, Term, true) :- atom(Space),
                                    function_type_annotation_term(Term, F, TypeChain), !,
                                    revise_function_type(
                                        Space, F,
                                        ( retractall(predeclared_function_type(Space, F, TypeChain)),
                                          remove_sexp(Space, Term) )).

%%Remove a function atom:
'remove-atom'(Space, Term, Removed) :- Term = [=,[F|Args],Body],
                                       atom(Space), atom(F), !,
                                       remove_sexp(Space, Term),
                                       space_pred(Space, F, Pred),
                                       catch(nb_getval(Pred, Prev), _, Prev = []),
                                       (   select(fun_meta(Args, Body), Prev, Rest)
                                           -> ( Rest == [] -> nb_delete(Pred)
                                                            ; nb_setval(Pred, Rest) ) ; true ),
                                       uninstall_function_clauses(Space, Term, Refs),
                                       metta_on_function_changed(Pred),
                                       invalidate_specializations(Pred),
                                       ( \+ ( current_predicate(Pred/A), functor(H2, Pred, A), clause(H2, _, _) )
                                         -> retractall(fun(Pred)),
                                            retractall(space_fun(Space, F, Pred)),
                                            metta_on_function_removed(Pred)
                                         ; true ),
                                       ( Refs = [] -> Removed = false ; Removed = true ).

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

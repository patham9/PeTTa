%Since both normal add-attom call and function additions needs to add the S-expression:
add_sexp(Space, [Rel|Args]) :- Term =.. [Space, Rel | Args],
                               maybe_debug_space(add, success, Space, [Rel|Args], _),
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
                                 assertz(Clause, Ref),
                                 assertz(translated_from(Ref, Term)),
                                 metta_on_function_changed(FAtom),
                                 invalidate_specializations(FAtom),
                                 maybe_print_compiled_clause("added function", Term, Clause).

%Add an atom to the space:
'add-atom'(Space, Term, true) :- add_sexp(Space, Term).

%%Remove a function atom:
'remove-atom'(Space, Term, Removed) :- Term = [=,[F|Args],Body], !,
                                       remove_sexp(Space, Term),
                                       catch(nb_getval(F, Prev), _, Prev = []),
                                       (   select(fun_meta(Args, Body), Prev, Rest)
                                           -> ( Rest == [] -> nb_delete(F)
                                                            ; nb_setval(F, Rest) ) ; true ),
                                       findall(Ref, translated_from(Ref, Term), Refs),
                                       forall(member(Ref, Refs), erase(Ref)),
                                       retractall(translated_from(_, Term)),
                                       metta_on_function_changed(F),
                                       invalidate_specializations(F),
                                       ( \+ ( current_predicate(F/A), functor(H2, F, A), clause(H2, _, _) )
                                         -> retractall(fun(F)), metta_on_function_removed(F)
                                         ; true ),
                                       ( Refs = [] -> Removed = false ; Removed = true ),
                                       maybe_debug_space(remove, Removed, Space, Term, Removed).

%Remove all same atoms:
'remove-atom'(Space, Term, true) :- remove_sexp(Space, Term),
                                    maybe_debug_space(remove, true, Space, Term, true).

%Match for conjunctive pattern
match(_, LComma, OutPattern, Result) :- LComma == [','], !,
                                        Result = OutPattern.
match(Space, [Comma|[Head|Tail]], OutPattern, Result) :- Comma == ',', !,
                                                         append([Space], Head, List),
                                                         Term =.. List,
                                                         ( catch(Term, _, fail),
                                                           \+ cyclic_term(OutPattern),
                                                           match(Space, [','|Tail], OutPattern, Result),
                                                           maybe_debug_space(match, success, Space, [','|[Head|Tail]], Result)
                                                         ; maybe_debug_space(match, fail, Space, [','|[Head|Tail]], _),
                                                           fail ).

% When the pattern list itself is a variable -> enumerate all atoms
match(Space, PatternVar, OutPattern, Result) :- var(PatternVar), !,
                                                maybe_debug_space(get_atoms, success, Space, PatternVar, _),
                                                'get-atoms'(Space, PatternVar),
                                                \+ cyclic_term(OutPattern),
                                                Result = OutPattern,
                                                maybe_debug_space(match, success, Space, PatternVar, Result).

%Match for pattern:
match(Space, [Rel|PatArgs], OutPattern, Result) :- Term =.. [Space, Rel | PatArgs],
                                                   ( catch(Term, _, fail),
                                                     \+ cyclic_term(OutPattern),
                                                     Result = OutPattern,
                                                     maybe_debug_space(match, success, Space, [Rel|PatArgs], Result)
                                                   ; maybe_debug_space(match, fail, Space, [Rel|PatArgs], _),
                                                     fail ).

%Get all atoms in space, irregard of arity:
'get-atoms'(Space, Pattern) :- current_predicate(Space/Arity),
                               functor(Head, Space, Arity),
                               clause(Head, true),
                               Head =.. [Space | Pattern].

maybe_debug_space(Op, Stage, Space, Pattern, Result) :-
    ( space_event_enabled(Op, Stage)
      -> emit_space_event(Op, Stage, Space, Pattern, Result)
      ; true
    ).

emit_space_event(add, _, Space, Term, _) :-
    debug_event(space, meta(space, 0, space), space(add, Space, Term)).
emit_space_event(remove, _, Space, Term, Removed) :-
    debug_event(space, meta(space, 0, space), space(remove, Space, Term, Removed)).
emit_space_event(match, Stage, Space, Pattern, Result) :-
    debug_event(space, meta(space, 0, space), space(match, Stage, Space, Pattern, Result)).
emit_space_event(get_atoms, _, Space, Pattern, _) :-
    debug_event(space, meta(space, 0, space), space(get_atoms, Space, Pattern)).

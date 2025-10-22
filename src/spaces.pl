%Arity expander for JIT-indexing-efficient representation of space entries:
ensure_dynamic_arity(Space,Arity) :- ( current_predicate(Space/Arity)
                                       -> true ; dynamic(Space/Arity) ).

%Since both normal add-attom call and function additions needs to add the S-expression:
add_sexp(Space, [Rel|Args]) :- length(Args, N), Arity is N + 2,
                               ensure_dynamic_arity(Space, Arity),
                               Term =.. [Space, Rel | Args],
                               assertz(Term).

%Add a function atom:
'add-atom'(Space, Term, true) :- Term = [=,[FAtom|_],_], !,
                                 add_sexp(Space, Term),
                                 register_fun(FAtom),
                                 translate_clause(Term, Clause),
                                 assertz(Clause).

%Add an atom to the space:
'add-atom'(Space, Term, true) :- add_sexp(Space, Term).

%%Remove a function atom:
'remove-atom'('&self', Term, Removed) :- Term = [=,[F|Ins],_], !,
                                         retractall(Term),
                                         translate_clause(Term, Cl),
                                         ( retract(Cl) -> length(Ins, K),
                                                          unregister_fun(F/K),
                                                          Removed=true
                                                        ; Removed=false ).

%Remove all same atoms:
'remove-atom'(Space, [Rel|Args], true) :- length(Args, N), Arity is N + 2,
                                          ensure_dynamic_arity(Space, Arity),
                                          Term =.. [Space, Rel | Args],
                                          retractall(Term).

%Match for conjunctive pattern
match(_, LComma, OutPattern, Result) :- LComma == [','], !,
                                        Result = OutPattern.
match(Space, [Comma|[Head|Tail]], OutPattern, Result) :- Comma == ',', !,
                                                         append([Space], Head, List),
                                                         Term =.. List,
                                                         Term, \+ cyclic_term(OutPattern),
                                                         match(Space, [','|Tail], OutPattern, Result).

% When the pattern list itself is a variable -> enumerate all atoms
match(Space, PatternVar, OutPattern, Result) :- var(PatternVar), !,
                                                'get-atoms'(Space, PatternVar),
                                                \+ cyclic_term(OutPattern),
                                                Result = OutPattern.

<<<<<<< HEAD
%Match for pattern:
match(Space, [Rel|PatArgs], OutPattern, Result) :- Term =.. [Space, Rel | PatArgs],
                                                   Term, \+ cyclic_term(OutPattern),
                                                   Result = OutPattern.
=======
% %Match for pattern:
% match(Space, [Rel|PatArgs], OutPattern, Result) :- Term =.. [Space, Rel | PatArgs],
%                                                    Term, \+ cyclic_term(OutPattern),
%                                                    copy_term(OutPattern, Result).

% 1. FIX: Universal Match for a single uninstantiated variable (like $x)
%    Avoids the arity error by redirecting to get-atoms, and uses call/3 for robustness.

match(Space, Pattern, Pattern, Result) :-
    var(Pattern), % Check if the pattern is an unbound variable (like $x)
    !,
    % Use findall and call/3 to execute the quoted predicate 'get-atoms'
    findall(Atom, call('get-atoms', Space, Atom), AllAtoms),
    Result = AllAtoms. 


% 2. Match for pattern (Handles non-variable/structured patterns like (Parent Tinsae $X))
match(Space, [Rel|PatArgs], OutPattern, Result) :- 
    Term =.. [Space, Rel | PatArgs],
    Term, \+ cyclic_term(OutPattern),
    copy_term(OutPattern, Result).
>>>>>>> 5130cbe (-fix standard name issue)

%Get all atoms in space, irregard of arity:
'get-atoms'(Space, Pattern) :- current_predicate(Space/Arity),
                               functor(Head, Space, Arity),
                               clause(Head, true),
                               Head =.. [Space | Pattern].

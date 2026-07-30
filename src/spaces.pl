%Runtime function add-atom cleans up its own staged raw atom, registration,
%metadata and compiled-dependency state on failure or exception. This remains
%a bounded transaction: an embedder observing concurrent mutation can still
%see the short interval between staging and cleanup, and a multi-function
%dependency cascade is not atomically rolled back as one unit.
:- discontiguous 'add-atom'/3.
%
%Since both normal add-attom call and function additions needs to add the S-expression:
add_sexp(Space, [Rel|Args]) :- Term =.. [Space, Rel | Args],
                               assertz(Term),
                               maybe_cache_type_decl(Space, [Rel|Args]).

%Same but for removal:
remove_sexp(Space, [Rel|Args]) :- Term =.. [Space, Rel | Args],
                                  retractall(Term),
                                  maybe_uncache_type_decl(Space, [Rel|Args]).

%Add a function atom:
'add-atom'(Space, Term, true) :- Term = [=,[FAtom|W],_], !,
                                 snapshot_runtime_function_add(FAtom, Snapshot),
                                 catch(
                                     ( runtime_add_function(Space, Term, FAtom, W,
                                                            RawRef, ClauseRef)
                                       -> true
                                     ; cleanup_runtime_function_add(
                                           FAtom, RawRef, ClauseRef, Snapshot),
                                       fail ),
                                     Error,
                                     ( cleanup_runtime_function_add(
                                           FAtom, RawRef, ClauseRef, Snapshot),
                                       throw(Error) )).

runtime_add_function(Space, Term, FAtom, W, RawRef, ClauseRef) :-
    Term = [=, [FAtom|W], TermBody],
    RawTerm =.. [Space, '=', [FAtom|W], TermBody],
    assertz(RawTerm, RawRef),
    maybe_cache_type_decl(Space, Term),
    register_fun(FAtom),
    length(W, N),
    Arity is N + 1,
    assertz(arity(FAtom, Arity)),
    once(translate_clause(Term, Clause, true, Dependencies)),
    assertz(Clause, ClauseRef),
    assertz(translated_from(ClauseRef, Term)),
    record_compiled_dependencies(ClauseRef, FAtom/N, Dependencies),
    notify_mutation(clause_changed(FAtom/N, runtime)),
    invalidate_specializations(FAtom),
    maybe_print_compiled_clause("added function", Term, Clause).

snapshot_runtime_function_add(F,
        runtime_add_snapshot(FunFacts, Arities, Recompile)) :-
    findall(true, fun(F), FunFacts),
    findall(A, arity(F, A), Arities),
    snapshot_recompile_state(F, Recompile).

cleanup_runtime_function_add(F, RawRef, ClauseRef,
        runtime_add_snapshot(FunFacts, Arities, Recompile)) :-
    ( nonvar(ClauseRef)
      -> forget_compiled_dependencies(ClauseRef),
         retractall(translated_from(ClauseRef, _)),
         ( clause(_, _, ClauseRef) -> erase(ClauseRef) ; true )
    ; true ),
    ( nonvar(RawRef), clause(_, _, RawRef) -> erase(RawRef) ; true ),
    restore_recompile_state(F, Recompile),
    retractall(fun(F)),
    forall(member(true, FunFacts), assertz(fun(F))),
    retractall(arity(F, _)),
    forall(member(A, Arities), assertz(arity(F, A))).

%Add an atom to the space:
'add-atom'(Space, Term, true) :-
    typed_space_runtime_value_ok(Space, Term),
    add_sexp(Space, Term).

%%Remove a function atom:
'remove-atom'(Space, Term, Removed) :- Term = [=,[F|Args],Body], !,
                                       remove_sexp(Space, Term),
                                       catch(nb_getval(F, Prev), _, Prev = []),
                                       (   select(Meta, Prev, Rest),
                                           fun_meta_parts(Meta, Args0, Body0, _),
                                           Args0 =@= Args,
                                           Body0 =@= Body
                                           -> ( Rest == [] -> nb_delete(F)
                                                            ; nb_setval(F, Rest) ) ; true ),
                                       findall(Ref, translated_from(Ref, Term), Refs),
                                       forall(member(Ref, Refs),
                                              ( forget_compiled_dependencies(Ref),
                                                erase(Ref) )),
                                       retractall(translated_from(_, Term)),
                                       metta_on_function_changed(F),
                                       invalidate_specializations(F),
                                       length(Args, N),
                                       notify_mutation(clause_changed(F/N, runtime)),
                                       ( \+ ( current_predicate(F/A), functor(H2, F, A), clause(H2, _, _) )
                                         -> retractall(fun(F)), metta_on_function_removed(F)
                                         ; true ),
                                       ( Refs = [] -> Removed = false ; Removed = true ).

%Remove all same atoms:
'remove-atom'(Space, Term, true) :-
    typed_space_runtime_value_ok(Space, Term),
    remove_sexp(Space, Term).

%Typed spaces accept open payloads and removal patterns.  At the operation
%boundary reject only a value that has become a definite contradiction; this
%is the runtime counterpart of translator.pl's check_typed_space_value/2, not
%a residual type guard, and it never constrains unresolved fields.
typed_space_runtime_value_ok(Space, Value) :-
    ( atom(Space), declared_space_type(Space, RowT),
      value_definitely_mismatch(Value, RowT)
      -> throw(error(literal_type_mismatch(Value, RowT), typecheck))
    ; true ).

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

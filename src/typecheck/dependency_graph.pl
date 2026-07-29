%%% Compiled-analysis dependency graph %%%
%
% Proof producers return dependencies; this file is the only publication and
% mutation boundary.  A compiled clause is keyed by its real Prolog clause
% reference, so removal and specialization invalidation cannot leave a live
% edge.  Whole-function queries are derived by unioning those clause records.
%
% Owns compiled/validation dependency stores and notify_mutation/1. Consumes
% proof-cache invalidation, canonical declaration queries, translator
% recompile/specialization interfaces, and subject-specific revalidators.
% Boundary: no other unit asserts the stores below.

:- dynamic compiled_deps/4.     % compiled_deps(ClauseRef, F/N, SourceFile, Dependencies)
:- dynamic compiled_dep_edge/3. % compiled_dep_edge(Dependency, ClauseRef, F/N)
:- dynamic validation_deps/2.   % validation_deps(ValidationKey, Dependencies)

record_compiled_dependencies(Ref, F/N, Dependencies) :-
    current_metta_file(File),
    record_compiled_dependencies(Ref, F/N, File, Dependencies).

record_compiled_dependencies(Ref, F/N, File, Dependencies) :-
    retractall(compiled_deps(Ref, _, _, _)),
    retractall(compiled_dep_edge(_, Ref, _)),
    copy_term_nat(Dependencies, Copy),
    sort(Copy, Deps),
    assertz(compiled_deps(Ref, F/N, File, Deps)),
    forall(member(Dependency, Deps),
           assertz(compiled_dep_edge(Dependency, Ref, F/N))).

forget_compiled_dependencies(Ref) :-
    retractall(compiled_deps(Ref, _, _, _)),
    retractall(compiled_dep_edge(_, Ref, _)).

compiled_dependency_origin(Ref, File) :-
    compiled_deps(Ref, _, File, _), !.
compiled_dependency_origin(_, File) :-
    current_metta_file(File).

compiled_function_dependencies(F, Dependencies) :-
    findall(D,
            ( compiled_deps(Ref, F/_, _, Ds),
              clause(_, _, Ref),
              member(D, Ds) ),
            Ds0),
    sort(Ds0, Dependencies).

recorded_constructor_dependency_types(Types) :-
    findall(T,
            ( compiled_dep_edge(ctor_set(T), Ref, _),
              clause(_, _, Ref),
              atom(T) ),
            Compiled),
    findall(T,
            ( validation_deps(_, Dependencies),
              member(ctor_set(T), Dependencies),
              atom(T) ),
            Validations),
    findall(T,
            ( analysis_memo(_, Proof),
              analysis_proof_dependencies(Proof, Dependencies),
              member(ctor_set(T), Dependencies),
              atom(T) ),
            Memos),
    append([Compiled, Validations, Memos], Ts0),
    sort(Ts0, Types).

record_validation_dependencies(Key, Dependencies) :-
    retractall(validation_deps(Key, _)),
    copy_term_nat(Dependencies, Copy),
    sort(Copy, Deps),
    assertz(validation_deps(Key, Deps)).

forget_validation_dependencies(Key) :-
    retractall(validation_deps(Key, _)).

% Mutation vocabulary:
%   clause_changed(F/N, prevalidated|runtime|derived)
%   declaration_changed(F/N, added|removed)
%   constructor_set_changed(Type, IntroducedOrRemovedSymbol)
%   alias_changed(Alias, added|removed, DiagnosticFunctions)
%   broad_mutation(Reason)                 % documented correctness fallback
%
% The broad form is a documented correctness fallback for a future mutation
% that cannot be mapped to a nominal or alias key. No current event needs it.
notify_mutation(clause_changed(FN)) :- !,
    notify_mutation(clause_changed(FN, runtime)).
notify_mutation(Event) :-
    mutation_seed_functions(Event, Seed),
    notify_mutation_queue([Event], graph_state(Seed, []), _).

notify_mutation_queue([], State, State).
notify_mutation_queue([Event|Events], State0, State) :-
    analysis_cache_invalidate_event(Event),
    affected_compiled_functions(Event, Functions),
    recompile_affected_functions(Functions, Event, State0, State1, MoreEvents),
    revalidate_affected_consumers(Event, State1, State2),
    append(MoreEvents, Events, Queue),
    notify_mutation_queue(Queue, State2, State).

%A source-load clause has already been validated against the file's complete
%prepass, and a derived event names the function the graph just rebuilt.  A
%runtime add/remove, however, must rebuild the changed function itself too:
%boundness provisos and commitment checks are clause-set unions whose emitted
%checks live in every clause.
mutation_seed_functions(clause_changed(F/_, prevalidated), [F]) :- !.
mutation_seed_functions(clause_changed(F/_, derived), [F]) :- !.
mutation_seed_functions(_, []).

affected_compiled_functions(Event, Functions) :-
    findall(F,
            ( mutation_candidate_dependency(Event, Dependency),
              compiled_dep_edge(Dependency, Ref, F/_),
              compiled_deps(Ref, _, File, _),
              clause(_, _, Ref),
              mutation_consumer_file_relevant(Event, File) ),
            Fs0),
    sort(Fs0, Sorted),
    prioritize_mutation_owner(Event, Sorted, Functions).

%The per-file prepass already made every definition and pending body in that
%same file visible. Recompiling its earlier clauses after each later clause is
%both redundant and observably different once specializations exist. A source
%clause still wakes consumers compiled in older files, which is the former
%late-symbol lifecycle.
mutation_consumer_file_relevant(clause_changed(_, prevalidated), File) :- !,
    current_metta_file(Current),
    File \== Current.
mutation_consumer_file_relevant(_, _).

mutation_candidate_dependency(clause_changed(F/N, prevalidated), late_call(F/N)).
mutation_candidate_dependency(clause_changed(F/_, prevalidated), late_symbol(F)).
mutation_candidate_dependency(clause_changed(F/N, Mode), effect(F/N)) :-
    Mode \== prevalidated.
mutation_candidate_dependency(clause_changed(F/N, Mode), decl(F/N)) :-
    Mode \== prevalidated.
mutation_candidate_dependency(clause_changed(F/N, Mode), clause_set(F/N)) :-
    Mode \== prevalidated.
mutation_candidate_dependency(clause_changed(F/N, Mode), output_cert(_, F/N)) :-
    Mode \== prevalidated.
mutation_candidate_dependency(clause_changed(F/N, Mode), late_call(F/N)) :-
    Mode \== prevalidated.
mutation_candidate_dependency(clause_changed(F/_, Mode), late_symbol(F)) :-
    Mode \== prevalidated.
mutation_candidate_dependency(declaration_changed(F/N, _), effect(F/N)).
mutation_candidate_dependency(declaration_changed(F/N, _), decl(F/N)).
mutation_candidate_dependency(declaration_changed(F/N, _), clause_set(F/N)).
mutation_candidate_dependency(constructor_set_changed(Type, _), ctor_set(Type)).
mutation_candidate_dependency(alias_changed(Alias, _, _), alias_expansion(Alias)).
mutation_candidate_dependency(alias_changed(Alias, _, _), ctor_set(Alias)).
mutation_candidate_dependency(broad_mutation(_), _).

prioritize_mutation_owner(declaration_changed(F/_, _), Functions, [F|Rest]) :-
    select(F, Functions, Rest), !.
prioritize_mutation_owner(clause_changed(F/_, runtime), Functions, [F|Rest]) :-
    select(F, Functions, Rest), !.
prioritize_mutation_owner(_, Functions, Functions).

recompile_affected_functions([], _, State, State, []).
recompile_affected_functions([F|Fs], Event,
                             graph_state(Visited0, Validated),
                             State, Events) :-
    ( memberchk(F, Visited0)
      -> recompile_affected_functions(Fs, Event,
                                      graph_state(Visited0, Validated),
                                      State, Events)
    ; mutation_recompile_diagnostic(Event, F),
      compiled_function_arities(F, BeforeArities),
      recompile_function_clauses(F),
      compiled_function_arities(F, AfterArities),
      append(BeforeArities, AfterArities, Ns0),
      sort(Ns0, Ns),
      findall(clause_changed(F/N, derived), member(N, Ns), FEvents),
      recompile_affected_functions(Fs, Event,
                                   graph_state([F|Visited0], Validated),
                                   State1, RestEvents),
      append(FEvents, RestEvents, Events),
      State = State1
    ).

compiled_function_arities(F, Arities) :-
    findall(N,
            ( compiled_deps(Ref, F/N, _, _),
              clause(_, _, Ref) ),
            Ns0),
    sort(Ns0, Arities).

revalidate_affected_consumers(Event,
                              graph_state(Fs, Validated0),
                              graph_state(Fs, Validated)) :-
    Event = clause_changed(_, prevalidated), !,
    Validated = Validated0.
revalidate_affected_consumers(Event,
                              graph_state(Fs, Validated0),
                              graph_state(Fs, Validated)) :-
    findall(Key,
            ( validation_deps(Key, Dependencies),
              member(Dependency, Dependencies),
              mutation_dependency_matches(Event, Dependency),
              \+ memberchk(Key, Validated0) ),
            Keys0),
    sort(Keys0, Keys),
    forall(member(Key, Keys), revalidate_dependency_consumer(Key, Event)),
    append(Keys, Validated0, Vs0),
    sort(Vs0, Validated).

% One matcher is shared by compiled consumers and memo proofs.
mutation_dependency_matches(clause_changed(F/N, prevalidated), late_call(F/N)).
mutation_dependency_matches(clause_changed(F/_, prevalidated), late_symbol(F)).
mutation_dependency_matches(clause_changed(F/N, Mode), effect(F/N)) :-
    Mode \== prevalidated.
mutation_dependency_matches(clause_changed(F/N, Mode), decl(F/N)) :-
    Mode \== prevalidated.
mutation_dependency_matches(clause_changed(F/N, Mode), clause_set(F/N)) :-
    Mode \== prevalidated.
mutation_dependency_matches(clause_changed(F/N, Mode), output_cert(_, F/N)) :-
    Mode \== prevalidated.
mutation_dependency_matches(clause_changed(F/N, Mode), late_call(F/N)) :-
    Mode \== prevalidated.
mutation_dependency_matches(clause_changed(F/_, Mode), late_symbol(F)) :-
    Mode \== prevalidated.
mutation_dependency_matches(declaration_changed(F/N, _), effect(F/N)).
mutation_dependency_matches(declaration_changed(F/N, _), decl(F/N)).
mutation_dependency_matches(declaration_changed(F/N, _), clause_set(F/N)).
mutation_dependency_matches(constructor_set_changed(Type, _), ctor_set(Type)).
mutation_dependency_matches(alias_changed(Alias, _, _), alias_expansion(Alias)).
% A declaration compiled before Alias existed necessarily saw it as a nominal
% type; matching that old key closes the late-alias direction as well.
mutation_dependency_matches(alias_changed(Alias, _, _), ctor_set(Alias)).
mutation_dependency_matches(broad_mutation(_), _).

mutation_recompile_diagnostic(constructor_set_changed(_, _), F) :- !,
    format(user_error,
           "Warning: a constructor declared after ~w was compiled changes a type it matched on; recompiling ~w~n",
           [F, F]).
mutation_recompile_diagnostic(alias_changed(Alias, added, DiagnosticFs), F) :-
    memberchk(F, DiagnosticFs), !,
    format(user_error,
           "Warning: type alias ~w arrives after declarations using it; recompiling ~w against the expanded type~n",
           [Alias, F]).
mutation_recompile_diagnostic(declaration_changed(F/N, added), F) :-
    compiled_function_arities(F, Ns),
    memberchk(N, Ns), !,
    format(user_error,
           "Warning: type declaration for ~w arrives after its definition; its clauses are being recompiled against it~n",
           [F]).
mutation_recompile_diagnostic(_, _).

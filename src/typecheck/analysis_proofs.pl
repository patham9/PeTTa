%%% Functional analysis records and the memo boundary.
%
% Analyses return one uniform, closed record:
%
%   analysis_proof(Subject, Verdict,
%                  requirements(BoundaryRequirements),
%                  certificates(OutputCertificates),
%                  dependencies(Dependencies))
%
% The core walkers communicate proof observations with delimited
% continuations, not dynamic facts or global accumulators.  A surrounding
% analysis_collect/2 turns those observations into ordinary returned data.
% Nested proof-producing analyses re-emit their observations to their caller,
% so a top-level proof contains the transitive evidence it consumed.

:- dynamic analysis_memo/2.

analysis_cache_lookup(Key, Proof) :-
    analysis_memo(Key, Stored),
    copy_term(Stored, Proof).

analysis_cache_store(Key, Proof) :-
    retractall(analysis_memo(Key, _)),
    copy_term(Proof, Stored),
    assertz(analysis_memo(Key, Stored)).

% Legacy selectors remain for callers which are not mutation boundaries.  A
% program mutation goes through analysis_cache_invalidate_event/1 below: it
% removes only proofs whose recorded (or memo-key-implied) dependencies match.
analysis_cache_invalidate(all) :-
    retractall(analysis_memo(_, _)).
analysis_cache_invalidate(det) :-
    retractall(analysis_memo(det(_, _), _)).
analysis_cache_invalidate(assume) :-
    retractall(analysis_memo(assume(_, _), _)).
analysis_cache_invalidate(effect) :-
    retractall(analysis_memo(effect(_, _, _), _)).
analysis_cache_invalidate(effect(F)) :-
    retractall(analysis_memo(effect(F, _, _), _)).
analysis_cache_invalidate(output) :-
    retractall(analysis_memo(output(_, _, _), _)).
analysis_cache_invalidate(output(Kind, F, N)) :-
    retractall(analysis_memo(output(Kind, F, N), _)).

analysis_cache_invalidate_event(Event) :-
    findall(Ref,
            ( clause(analysis_memo(Key, Proof), true, Ref),
              analysis_memo_dependencies(Key, Proof, Dependencies),
              member(Dependency, Dependencies),
              mutation_dependency_matches(Event, Dependency) ),
            Refs0),
    sort(Refs0, Refs),
    forall(member(Ref, Refs), erase(Ref)).

analysis_cache_invalidate_outputs(F) :-
    retractall(analysis_memo(output(_, F, _), _)).

analysis_memo_dependencies(Key, Proof, Dependencies) :-
    analysis_proof_dependencies(Proof, Recorded),
    analysis_memo_key_dependencies(Key, Implied),
    append(Recorded, Implied, Ds0),
    sort(Ds0, Dependencies).

analysis_memo_key_dependencies(det(F, N), Ds) :- !,
    Ds = [effect(F/N), decl(F/N), clause_set(F/N)].
analysis_memo_key_dependencies(assume(F, N), Ds) :- !,
    Ds = [effect(F/N), decl(F/N), clause_set(F/N)].
analysis_memo_key_dependencies(effect(F, N, _), Ds) :- !,
    Ds = [effect(F/N), decl(F/N), clause_set(F/N)].
analysis_memo_key_dependencies(output(Kind, F, N), Ds) :- !,
    Ds = [output_cert(Kind, F/N), clause_set(F/N)].
analysis_memo_key_dependencies(_, []).

analysis_emit(Event) :-
    catch(shift(analysis_event(Event)),
          error(existence_error(reset, _), _),
          true).

analysis_collect(Goal, Events) :-
    reset(Goal, Ball, Continuation),
    analysis_collect_continuation(Ball, Continuation, Events).

analysis_collect_continuation(_, 0, []) :- !.
analysis_collect_continuation(analysis_event(Event), Continuation, [Event|Events]) :- !,
    analysis_collect(Continuation, Events).
analysis_collect_continuation(Ball, _, _) :-
    throw(error(unhandled_analysis_shift(Ball), analysis)).

analysis_make_proof(Subject, Verdict, Events, ExtraDependencies, Proof) :-
    findall(bound(Pos, Kind), member(required_bound(Pos, Kind), Events), Bs0),
    findall(output(Kind, Key), member(certificate(Kind, Key), Events), Cs0),
    findall(D, member(dependency(D), Events), Ds0),
    append(Ds0, ExtraDependencies, Ds1),
    sort(Bs0, Bounds),
    sort(Cs0, Certs),
    sort(Ds1, Deps),
    Proof = analysis_proof(Subject, Verdict,
                           requirements(Bounds),
                           certificates(Certs),
                           dependencies(Deps)).

analysis_proof_verdict(analysis_proof(_, Verdict, _, _, _), Verdict).
analysis_proof_requirements(analysis_proof(_, _, requirements(Bounds), _, _), Bounds).
analysis_proof_certificates(analysis_proof(_, _, _, certificates(Certs), _), Certs).
analysis_proof_dependencies(analysis_proof(_, _, _, _, dependencies(Deps)), Deps).

analysis_reemit_proof(Proof) :-
    analysis_proof_certificates(Proof, Certs),
    analysis_proof_dependencies(Proof, Deps),
    %Boundary requirements belong to Proof's subject.  A caller may depend on
    %the callee proof, but must never reinterpret the callee's argument
    %positions as its own; only the compile/validation boundary publishes them.
    forall(member(output(Kind, Key), Certs),
           analysis_emit(certificate(Kind, Key))),
    forall(member(Dep, Deps),
           analysis_emit(dependency(Dep))).

% Conservative dependency inventory for an expression or clause-set term.
% Every real named call records its declaration/effect and both certificate
% families. A declared-but-as-yet undefined call or bare symbol instead gets a
% late_* dependency, so its first definition invalidates that decision without
% waking ordinary same-file callers already covered by the declaration/body
% prepasses.
% Type dependencies are attached only when the term actually carries them;
% the old "all types in the program" approximation would make the Phase-4
% dependency graph effectively global on every constructor declaration.
analysis_term_dependencies(Term, Dependencies) :-
    analysis_term_calls(Term, Calls0),
    sort(Calls0, Calls),
    findall(D,
            ( member(F/N, Calls),
              analysis_call_dependency(F, N, D) ),
            CallDeps),
    analysis_term_symbols(Term, Symbols0),
    sort(Symbols0, Symbols),
    findall(late_symbol(S),
            ( member(S, Symbols),
              \+ fun(S),
              fn_decl_arity(S, _, _, _) ),
            SymbolDeps),
    analysis_known_type_dependencies(Term, KnownTypeDeps),
    append([CallDeps, SymbolDeps, KnownTypeDeps], Ds0),
    sort(Ds0, Dependencies).

analysis_call_dependency(F, N, D) :-
    fun(F), !,
    member(D, [effect(F/N), decl(F/N),
               output_cert(proper_list, F/N),
               output_cert(bound_bool, F/N)]).
analysis_call_dependency(F, N, late_call(F/N)) :-
    fn_decl_arity(F, N, _, _).

analysis_known_type_dependencies(Term, Dependencies) :-
    term_variables(Term, Vars),
    findall(D,
            ( member(V, Vars),
              get_attr(V, tknown, Types),
              member(T, Types),
              analysis_type_dependency(T, D) ),
            Ds0),
    sort(Ds0, Dependencies).

analysis_function_decl_dependencies(F, Dependencies) :-
    findall(Scheme-Provenance,
            fn_decl_copy(F, _, Scheme, _, _, Provenance),
            Declarations),
    findall(D,
            ( member(scheme(ATs, OT)-_, Declarations),
              member(Term, [ATs, OT]),
              analysis_type_dependency(Term, D) ),
            NormalizedDeps),
    findall(D,
            ( member(_-provenance(_, syntax(Syntax)), Declarations),
              analysis_type_dependency(Syntax, D) ),
            SourceDeps),
    append(NormalizedDeps, SourceDeps, Ds0),
    sort(Ds0, Dependencies).

analysis_type_dependency(T, alias_expansion(T)) :-
    atom(T), declared_type_alias(T, _).
analysis_type_dependency(T, ctor_set(T)) :-
    atom(T), \+ primitive_type(T), \+ wildcard_type(T),
    \+ declared_type_alias(T, _).
analysis_type_dependency(T, D) :-
    nonvar(T), is_list(T), member(E, T), analysis_type_dependency(E, D).

analysis_term_calls(Term, Calls) :-
    ( var(Term) -> Calls = []
    ; atomic(Term) -> Calls = []
    ; Term = [F|Args], atom(F), is_list(Args)
      -> length(Args, N),
         maplist(analysis_term_calls, Args, Nested),
         append([[F/N]|Nested], Calls)
    ; Term = [H|T]
      -> analysis_term_calls(H, HC),
         analysis_term_calls(T, TC),
         append(HC, TC, Calls)
    ; compound(Term)
      -> Term =.. [_|Args],
         maplist(analysis_term_calls, Args, Nested),
         append(Nested, Calls)
    ; Calls = [] ).

analysis_term_symbols(Term, Symbols) :-
    ( var(Term) -> Symbols = []
    ; atom(Term) -> Symbols = [Term]
    ; atomic(Term) -> Symbols = []
    ; Term = [H|T]
      -> analysis_term_symbols(H, HS),
         analysis_term_symbols(T, TS),
         append(HS, TS, Symbols)
    ; compound(Term)
      -> Term =.. [_|Args],
         maplist(analysis_term_symbols, Args, Nested),
         append(Nested, Symbols)
    ; Symbols = [] ).

analysis_snapshot_proof(Subject, Snapshot, Dependencies,
                        analysis_proof(Subject, snapshot(Snapshot),
                                       requirements([]), certificates([]),
                                       dependencies(Deps))) :-
    sort(Dependencies, Deps).

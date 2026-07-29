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

% Phase 3 deliberately preserves the old broad invalidation policy.  Phase 4
% can refine this one boundary using the dependencies already carried by every
% proof, without finding cache writes scattered through the analyzers again.
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

analysis_cache_invalidate_clause_change :-
    analysis_cache_invalidate(det),
    analysis_cache_invalidate(assume),
    analysis_cache_invalidate(effect),
    analysis_cache_invalidate(output).

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
% This is intentionally an over-approximation for Phase 4: every named call
% records its declaration/effect and both output-certificate families, while
% all currently declared nominal/alias types are included because parameter
% attributes and normalized schemes can make them relevant without leaving a
% syntactic type atom in the expression.
analysis_term_dependencies(Term, Dependencies) :-
    analysis_term_calls(Term, Calls0),
    sort(Calls0, Calls),
    findall(D,
            ( member(F/N, Calls),
              member(D, [effect(F/N), decl(F/N),
                         output_cert(proper_list, F/N),
                         output_cert(bound_bool, F/N)]) ),
            CallDeps),
    findall(ctor_set(T), declared_newtype(T, _), CtorDeps),
    findall(ctor_set(T),
            ( declared_value_type(_, T), atom(T),
              \+ primitive_type(T), \+ wildcard_type(T) ),
            ValueTypeDeps),
    findall(alias_expansion(A), declared_type_alias(A, _), AliasDeps),
    analysis_known_type_dependencies(Term, KnownTypeDeps),
    append([CallDeps, CtorDeps, ValueTypeDeps, AliasDeps, KnownTypeDeps], Ds0),
    sort(Ds0, Dependencies).

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
    findall(Scheme,
            fn_decl_copy(F, _, Scheme, _, _, _),
            Schemes),
    findall(D,
            ( member(scheme(ATs, OT), Schemes),
              member(Term, [ATs, OT]),
              analysis_type_dependency(Term, D) ),
            Ds0),
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
    ; Calls = [] ).

analysis_snapshot_proof(Subject, Snapshot, Dependencies,
                        analysis_proof(Subject, snapshot(Snapshot),
                                       requirements([]), certificates([]),
                                       dependencies(Deps))) :-
    sort(Dependencies, Deps).

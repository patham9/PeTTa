:- dynamic translated_from/2.

%Pattern matching, structural and functional/relational constraints on arguments:
constrain_args(X, X, []) :- (var(X); atomic(X)), !.
constrain_args([F, A, B], Out, Goals) :- nonvar(F),
                                         F == cons,
                                         constrain_args(A, A1, G1),
                                         constrain_args(B, B1, G2),
                                         Out = [A1|B1],
                                         append(G1, G2, Goals), !.
constrain_args([F|Args], Var, Goals) :- atom(F),
                                        fun(F), !,
                                        translate_expr([F|Args], GoalsExpr, Var),
                                        flatten(GoalsExpr, Goals).
constrain_args(In, Out, Goals) :- maplist(constrain_args, In, Out, NestedGoalsList),
                                  flatten(NestedGoalsList, Goals), !.

%Flatten (= Head Body) MeTTa function into Prolog Clause. The wrapper records
%which types' CONSTRUCTOR SETS this clause's compilation read, so a
%constructor declared later can find the clauses it invalidates
%(with_ctor_snapshot/2, typecheck.pl). Specialized copies go through the
%3-argument form and record nothing: they are instances of a clause that
%already carries the dependency.
translate_clause(Input, (Head :- BodyConj)) :-
        ( nonvar(Input), Input = [Eq, H0, _], Eq == (=), nonvar(H0), H0 = [F0|_], atom(F0)
          -> F = F0 ; F = '$no_function' ),
        with_ctor_snapshot(F, translate_clause(Input, (Head :- BodyConj), true)).
translate_clause(Input, (Head :- BodyConj), ConstrainArgs) :-
                                               Input = [=, [F|Args0], BodyExpr],
                                               ( ConstrainArgs -> maplist(constrain_args, Args0, Args1, GoalsA),
                                                                  flatten(GoalsA,GoalsPrefix)
                                                                ; Args1 = Args0, GoalsPrefix = [] ),
                                               catch(nb_getval(F, Prev), _, Prev = []),
                                               nb_setval(F, [fun_meta(Args1, BodyExpr) | Prev]),
                                               analysis_cache_invalidate_clause_change, %all analysis memos depend on clause sets
                                               %clause set changed: the output-certificate memos (proper_list,
                                               %bound_bool) depend on clause sets transitively, so they reset
                                               %alongside the det caches above (see output_cert/3):
                                               reset_output_certs(F),
                                               clause_param_types(F, Args1, DeclOut),
                                               %Snapshot the declared arg positions that stay bare type variables after
                                               %head binding; checked below to enforce their claimed universality:
                                               parametric_param_snapshot(DeclOut, ParamVars),
                                               %...and publish them for the body compile: they are
                                               %promises to the caller, so neither a discharge nor a
                                               %compiler guess may treat one as knowledge. A specialized
                                               %copy promises nothing new (it is an instance):
                                               ( ConstrainArgs == false -> Promises = [] ; Promises = ParamVars ),
                                               param_promises_scope(Promises, OuterPromises),
                                               %Specialized clause copies (ConstrainArgs == false) are instances of
                                               %already-validated clauses: bind their (more specific) param types for
                                               %guard elimination, but skip the determinism/strict/output checks.
                                               %Determinism runs after param binding so closure params carry their
                                               %declared arrow types into the body analysis.
                                               ( ConstrainArgs == false -> true
                                                                         ; validate_function_determinism(F, Args1, BodyExpr, Prev) ),
                                               %A function whose declaration carries an EXPLICIT -[det]->/-[semidet]->
                                               %arrow promises, in EVERY mode, that it is called with bound arguments -
                                               %but only for the parameters its determinism proof CONSUMED. Emit those
                                               %runtime boundness checks now, AFTER validation has populated the
                                               %det_bound_proviso union, while the param vars are still fresh; they are
                                               %spliced in before the commit cut below (goal-term position unchanged).
                                               det_boundness_checks(F, Args1, DetChecks),
                                               begin_clause_inference(F, Args1, Assume, SavedInf),
                                               translate_declared_body(DeclOut, BodyExpr, GoalsBody, ExpOut),
                                               %A body that forced a declared parametric parameter to a concrete type
                                               %broke the universality its declaration claims (skip for specialized copies):
                                               ( ConstrainArgs == false -> true
                                                                         ; parametric_param_check(F, ParamVars) ),
                                               (  nonvar(ExpOut) , ExpOut = partial(Base,Bound)
                                               -> arity(Base, Arity), length(Bound, N), M is (Arity - N) - 1,
                                                  length(ExtraArgs, M), append([Bound,ExtraArgs,[Out]],CallArgs), Goal =.. [Base|CallArgs],
                                                  append(GoalsBody,[Goal],FinalGoals), append(Args1,ExtraArgs,HeadArgs),
                                                  OutChecks = [],
                                                  end_clause_inference(F, Args1, none, none, SavedInf)
                                               ; FinalGoals= GoalsBody , HeadArgs = Args1, Out = ExpOut,
                                                 end_clause_inference(F, Args1, ExpOut, Assume, SavedInf),
                                                 %The output certification runs for a SPECIALIZED copy too.
                                                 %It is an instance, so it can only make the result type more
                                                 %specific - it either discharges statically (one guard less),
                                                 %reproduces the general clause's guard, or finds a definite
                                                 %conflict, in which case the typecheck error means "do not
                                                 %specialize" and the call falls back to the general, guarded
                                                 %clause. Skipping it let the specializer DROP a guard the
                                                 %general clause was compiled with, which is the one outcome
                                                 %that is not sound:
                                                 clause_output_goals(F, DeclOut, ExpOut, BodyExpr, OutChecks0),
                                                 oracle_output_check(DeclOut, Out, OutChecks0, OutChecks) ),
                                               ( ConstrainArgs == false -> true
                                                                         ; strict_check_function_typed(F, Args1) ),
                                               append(HeadArgs, [Out], FinalArgs),
                                               Head =.. [F|FinalArgs],
                                               length(FinalArgs, CompiledArity),
                                               (arity(F, CompiledArity) -> true ; assertz(arity(F, CompiledArity))),
                                               %declared-deterministic functions commit to the first matching
                                               %clause (non-overlap is validated), guaranteeing no choicepoints
                                               %and enabling last-call optimization. -[semidet]-> commits
                                               %identically: allowing failure costs no choicepoint, so a
                                               %semidet function is as cheap as a det one:
                                               ( clause_commit_cut(F, Args1) -> Commit = [!] ; Commit = [] ),
                                               param_promises_restore(OuterPromises),
                                               append([DetChecks, GoalsPrefix, Commit, FinalGoals, OutChecks], Goals),
                                               goals_list_to_conj(Goals, BodyConj).

%%% Committed-arrow BOUNDNESS enforcement, NEED-BASED. Explicit det/semidet
%%% arrows are every-mode promises; a plain arrow participates while
%%% --strict-det makes it a commitment. A parameter is
%%% checked nonvar on entry ONLY when the clause-set's determinism proof actually
%%% CONSUMED its boundness - i.e. enforced_bound_param/1 succeeded on it during a
%%% call-site strengthening (manifest_bool, enforced_bound_nominal,
%%% enforced_bound_tuple, is-member probe). The emitted check is then exactly the
%%% proviso the certificate relied on: a pure data constructor
%%% ((= (pair-up $x $y) ($x $y))) is genuinely det with unbound args, consumes no
%%% boundness, and gets NO check. Where a check IS emitted it throws a clear error
%%% where the code previously enumerated a finite type or crashed in a builtin.
%%%
%%% The consumed positions are the per-function UNION
%%% det_bound_proviso(F,N,Pos,Kind),
%%% populated by validation (validate_function_determinism, run just before this).
%%% A param consumed by ANY clause is checked in EVERY clause of the function -
%%% a sound superset of the strict per-clause need (an extra check never unsound).
%%%
%%% SPINE-LEVEL, deliberately: enforced_bound_param/1 records DIRECT params only
%%% (a destructured FIELD is skipped by det_direct_param/1), so a field is never a
%%% proviso and never checked. Chainer proof terms legitimately carry unbound vars
%%% inside otherwise-bound data, so field-level enforcement would reject them. The
%%% is-var exemption is preserved for free: det_enforced_params/3 keeps an is-var
%%% param out of the published direct params, so enforced_bound_param/1 can never
%%% consume it and no proviso is ever recorded for it.
%%%
%%% Specialized clause copies (ConstrainArgs == false) get the same checks -
%%% calls route directly to them - reading the union the general clauses filled;
%%% a position bound by specialization is nonvar here and simply skipped. The
%%% late-declaration recompile clears and re-derives the union, then re-emits.
%The consumed POSITIONS are collected first (ground integers, so findall's copy
%is harmless), then the check goals are built OUTSIDE findall so each nonvar
%guard shares the actual head-argument variable - collecting the goals through
%findall would copy that variable and guard a disconnected fresh one instead.
det_boundness_checks(F, Args, Checks) :-
    ( length(Args, N), boundary_commitment(F, N, Det)
      -> findall(Pos, det_bound_proviso(F, N, Pos, _), Ps0),
         sort(Ps0, Ps),
         det_pos_checks(Ps, F, N, Det, Args, Checks)
       ; Checks = [] ).

det_pos_checks([], _, _, _, _, []).
det_pos_checks([Pos|Ps], F, N, Det, Args, Checks) :-
    nth1(Pos, Args, A),
    ( var(A) -> det_param_check(F, N, Pos, Det, A, Chk), Checks = [Chk|Rest]
              ; Checks = Rest ),
    det_pos_checks(Ps, F, N, Det, Args, Rest).

det_param_check(F, N, Pos, Det, A, Check) :-
    ( det_bound_proviso(F, N, Pos, proper_list)
      -> Check = ( is_list(A) -> true
                 ; throw(error(det_argument_not_proper_list(F, Det), determinism)) )
    ; Check = ( nonvar(A) -> true
              ; throw(error(unbound_det_argument(F, Det), determinism)) ) ).

clause_commit_cut(F, Args) :- \+ suppress_det_cut(true),
                              length(Args, N),
                              catch(( fn_determinism(F, N, D), committed_det(D) ), _, fail).

%%% Late binding across files. A call to a declared function whose definition
%%% has not arrived yet compiles as data (which is also what keeps declared
%%% constructors literal). Clauses embedding such a symbol are recorded, and
%%% when the definition arrives they are retranslated - now as real calls.
:- dynamic late_symbol_use/3.   % late_symbol_use(F, ClauseRef, SourceTerm)

note_late_symbol_uses(Term, Ref) :- Term = [=, _, Body],
                                    forall(( declared_undefined_atom(Body, F),
                                             \+ late_symbol_use(F, Ref, _) ),
                                           assertz(late_symbol_use(F, Ref, Term))).

declared_undefined_atom(T, F) :- ( atom(T) -> F = T, \+ fun(F), fn_decl_arity(F, _, _, _)
                                 ; is_list(T), member(E, T), declared_undefined_atom(E, F) ).

%The definition of F arrived: retranslate every clause that saw F as data.
%The stale fun_meta entry is dropped first so the clause is not re-validated
%against itself; its specializations are invalidated like any redefinition.
recompile_late_uses(F) :- ( late_symbol_use(F, _, _)
                            -> findall(Ref-Term, late_symbol_use(F, Ref, Term), Us),
                               retractall(late_symbol_use(F, _, _)),
                               forall(member(Ref-Term, Us), recompile_clause(Ref, Term))
                             ; true ).

recompile_clause(Ref, Term) :- ( clause(_, _, Ref)
                                 -> erase(Ref),
                                    retractall(translated_from(Ref, _)),
                                    Term = [=, [G|_], Body],
                                    drop_stale_fun_meta(G, Body),
                                    translate_clause(Term, Clause),
                                    assertz(Clause, NewRef),
                                    assertz(translated_from(NewRef, Term)),
                                    note_late_symbol_uses(Term, NewRef),
                                    invalidate_specializations(G)
                                  ; true ).

drop_stale_fun_meta(G, Body) :- catch(nb_getval(G, Metas), _, fail),
                                select(fun_meta(_, B), Metas, Rest),
                                attribute_free_variant(B, Body), !,
                                nb_setval(G, Rest).
drop_stale_fun_meta(_, _).

%%% Recompiling a whole function, for knowledge that arrived AFTER its clauses
%%% were compiled. Two things can do that, and both used to be believed and
%%% never enforced:
%%%
%%%   - a type/determinism DECLARATION in a later file than the definition it
%%%     constrains (typecheck.pl, maybe_cache_type_decl/2). The clauses were
%%%     validated and emitted with no declaration in sight, so they carry no
%%%     determinism commit and no argument/output certification.
%%%   - a CONSTRUCTOR declared in a later file than a clause whose compilation
%%%     read that type's constructor set (typecheck.pl, ctor_snapshot_use/3).
%%%
%%% Unlike recompile_late_uses/1 - which revisits clauses one at a time, each
%%% independent of the others - a function's clauses must be redone TOGETHER
%%% and IN SOURCE ORDER: the determinism overlap check compares each clause
%%% against the ones before it, so the meta list has to be rebuilt from empty
%%% in the original order or the check sees the wrong predecessors. That is
%%% the only difference; the per-clause work is recompile_clause/2 as usual.
%%%
%%% Errors are not swallowed. A late -[det]-> over clauses that genuinely
%%% overlap throws exactly the error the declaration would have caused had it
%%% arrived first - which is the whole point: enforced, or rejected.
recompile_function_clauses(F) :- function_source_clauses(F, Us),
                                 analysis_cache_invalidate(effect(F)),
                                 ( Us == [] -> true
                                 ; retractall(det_bound_proviso(F, _, _, _)),  %re-derive the boundness union from scratch
                                   reset_output_certs(F),  %withdraw the output certificates for re-derivation
                                   nb_setval(F, []),
                                   forall(member(Ref-Term, Us), recompile_clause(Ref, Term)) ).

%Every compiled clause of F, in the order it was asserted (which is source
%order - process_form/3 records translated_from/2 as it goes):
function_source_clauses(F, Us) :- findall(Ref-Term,
                                          ( translated_from(Ref, Term), nonvar(Term),
                                            Term = [=, Head, _], nonvar(Head), Head = [F0|_], F0 == F,
                                            clause(_, _, Ref) ),
                                          Us).

%%% A runtime clause change invalidates transitive determinism proofs already
%%% baked into callers. Walk recorded source terms to find named call sites,
%%% recompile each caller with the changed callee visible, and continue only
%%% when the caller's compiled form actually changed. The visited set breaks
%%% recursion; recompile_function_clauses/1 supplies validation, cache reset,
%%% and specialization invalidation. Errors deliberately propagate exactly as
%%% they would during a fresh compilation.
metta_on_function_changed(F) :- recompile_changed_callers([F], F).

recompile_changed_callers(_, F) :-
    direct_compiled_callers(F, Callers),
    Callers == [], !.
recompile_changed_callers(Visited, F) :-
    direct_compiled_callers(F, Callers),
    recompile_changed_caller_list(Callers, Visited).

recompile_changed_caller_list([], _).
recompile_changed_caller_list([G|Gs], Visited) :-
    ( memberchk(G, Visited)
      -> Visited1 = Visited
    ; compiled_function_snapshot_proof(G, BeforeProof),
      recompile_function_clauses(G),
      compiled_function_snapshot_proof(G, AfterProof),
      analysis_proof_verdict(BeforeProof, snapshot(Before)),
      analysis_proof_verdict(AfterProof, snapshot(After)),
      ( attribute_free_variant(Before, After)
        -> Visited1 = [G|Visited]
      ; recompile_changed_callers([G|Visited], G),
        Visited1 = [G|Visited] ) ),
    recompile_changed_caller_list(Gs, Visited1).

direct_compiled_callers(F, Callers) :-
    findall(G,
            ( translated_from(Ref, Term), clause(_, _, Ref),
              Term = [=, Head, Body], nonvar(Head), Head = [G|_],
              G \== F, source_calls_named(Body, F) ),
            Gs0),
    sort(Gs0, Callers).

source_calls_named(E, F) :-
    nonvar(E), is_list(E), E = [H|Args],
    ( atom(H), H == F
    ; member(A, Args), source_calls_named(A, F) ).

compiled_function_snapshot(F, Snapshot) :-
    compiled_function_snapshot_proof(F, Proof),
    analysis_proof_verdict(Proof, snapshot(Snapshot)).

compiled_function_snapshot_proof(F, Proof) :-
    findall((H :- B),
            ( translated_from(Ref, Term), clause(H, B, Ref),
              Term = [=, Head, _], nonvar(Head), Head = [F0|_], F0 == F ),
            Clauses),
    copy_term_nat(Clauses, Snapshot),
    numbervars(Snapshot, 0, _, [singletons(true)]),
    analysis_term_dependencies(Clauses, Dependencies),
    analysis_snapshot_proof(compiled(F), Snapshot, Dependencies, Proof).

%Print compiled clause:
maybe_print_compiled_clause(_, _, _) :- silent(true), !.
maybe_print_compiled_clause(Label, FormTerm, Clause) :-
    swrite(FormTerm, FormStr),
    format("\e[33m-->  ~w  -->~n\e[36m~w~n\e[33m--> prolog clause -->~n\e[32m", [Label, FormStr]),
    portray_clause(current_output, Clause),
    format("\e[33m^^^^^^^^^^^^^^^^^^^^^~n\e[0m").

%Conjunction builder, turning goals list to a flat conjunction:
goals_list_to_conj([], true)      :- !.
goals_list_to_conj([G], G)        :- !.
goals_list_to_conj([G|Gs], (G,R)) :- goals_list_to_conj(Gs, R).

incomplete_application_kind(Fun, Arity, partial) :- ( arity(Fun, KnownArity), KnownArity >= Arity
                                                     ; \+ arity(Fun, _) ), !.
incomplete_application_kind(_, _, overapplied).

throw_function_overapplication(Fun, ActualInputArity) :-
    findall(InputArity, (arity(Fun, Arity), InputArity is Arity - 1), InputArities),
    sort(InputArities, KnownInputArities),
    throw(error(domain_error(function_input_arities(Fun, KnownInputArities), ActualInputArity), none)).

% Runtime dispatcher: call F if it's a registered fun/1, else keep as list:
reduce([F|Args], Out) :- nonvar(F), atom(F), fun(F)
                         -> % --- Case 1: callable predicate ---
                            length(Args, N),
                            Arity is N + 1,
                            ( current_predicate(F/Arity) , \+ (current_op(_, _, F), Arity =< 2)
                              -> append(Args,[Out],CallArgs),
                                 Goal =.. [F|CallArgs],
                                 catch(call(Goal),_,fail)
                            ; incomplete_application_kind(F, Arity, partial)
                              -> Out = partial(F,Args)
                               ; throw_function_overapplication(F, N) )
                          ; % --- Case 2: partial closure ---
                            compound(F), F = partial(Base, Bound) -> append(Bound, Args, NewArgs),
                                                                     reduce([Base|NewArgs], Out)
                          ; % --- Case 3: leave unevaluated ---
                            Out = [F|Args],
                            \+ cyclic_term(Out).

%Calling reduce from aggregate function foldall needs this argument wrapping
agg_reduce(AF, Acc, Val, NewAcc) :- reduce([AF, Acc, Val], NewAcc).

%Combined expr translation to goals list
translate_expr_to_conj(Input, Conj, Out) :- translate_expr(Input, Goals, Out),
                                            goals_list_to_conj(Goals, Conj).

%%% Narrow bidirectional typing for declared product/list results.
%
%The ordinary translator is bottom-up. At a declared function boundary we
%also know the expected result type, so positional tuples and runtime lists can
%be checked element-by-element while constructed. The expectation follows
%result positions through if/case, match, and let/let*, but deliberately no
%farther: this is not a general inference rewrite, and folds retain their
%existing rules.
%
%Only POSITIONAL products and (List T) participate. A tagged structural type
%has a literal runtime tag and remains constructor-checked by the ordinary
%bottom-up path; `data` itself remains product-only.
translate_declared_body(out(OT, _), Expr, Goals, Out) :-
        contextual_expected_type(OT), !,
        translate_expected_product(Expr, OT, Goals, Out).
translate_declared_body(_, Expr, Goals, Out) :- translate_expr(Expr, Goals, Out).

contextual_product_type(T) :- nonvar(T), is_list(T),
                              \+ special_compound_type(T),
                              \+ tagged_tuple_type(T, _, _).

%The exact constructed shapes eligible to receive a top-down expectation.
%Keeping this separate preserves contextual_product_type/1's tuple meaning.
contextual_expected_type(T) :- ( contextual_product_type(T)
                               ; nonvar(T), list_type(T, _) ).

%Do not let an expression variable unify with one of the syntax patterns
%below. Besides changing the source term, that fabricated a `(data ...)`
%construction around nested product parameters and forced checks on phantom
%fields. Variables and atoms have no result-position structure to descend.
%Likewise, a compound with a variable head is DATA whose head is unbound, not
%a syntax form: letting a literal clause head bind it turned the real
%`($proof)` singleton into `(make-list)` under an expected (List Proof).
translate_expected_product(Expr, Expected, [], Out) :-
        nonvar(Expr), Expr == [],
        list_type(Expected, _), !,
        Out = [],
        set_out_type(Out, Expected).
translate_expected_product(Expr, _, Goals, Out) :-
        ( var(Expr) ; atomic(Expr) ; Expr = partial(_, _) ), !,
        translate_expr(Expr, Goals, Out).
translate_expected_product(Expr, _, Goals, Out) :-
        nonvar(Expr), Expr = [H|_], \+ atom(H), !,
        translate_expr(Expr, Goals, Out).
translate_expected_product([match, Space, Pattern, Body], Expected, Goals, Out) :- !,
        translate_expr(Space, G1, S),
        type_match_pattern(Pattern),
        bind_typed_space_pattern(Space, Pattern),
        translate_expected_product(Body, Expected, GsB, Out),
        append(G1, [match(S, Pattern, Out, Out)], G2),
        append(G2, GsB, Goals).
translate_expected_product([data|Fields], Expected, Goals, Out) :-
        contextual_product_type(Expected),
        same_length(Fields, Expected), !,
        translate_explicit_data(Fields, expected(Expected), Goals, Out).
translate_expected_product([Cons, H, Tl], Expected, Goals, Out) :-
        list_type(Expected, ET),
        ( Cons == cons ; Cons == 'cons-atom' ), !,
        translate_expected_list_element(H, ET, Cons, GsH, HV),
        translate_expected_product(Tl, Expected, GsT, TV),
        check_call_arg(declared, Cons, TV, Expected, TailChecks),
        append([GsH, GsT, TailChecks], Inner),
        build_direct_call(Cons, [HV, TV], Out, Inner, [], Goals),
        set_out_type(Out, Expected).
translate_expected_product(['make-list'|Elements], Expected, Goals, Out) :-
        list_type(Expected, ET), !,
        translate_explicit_list(Elements, expected(ET), Goals, Out).
translate_expected_product([if, Cond, Then], Expected, Goals, Out) :- !,
        translate_if_cond(Cond, ConC, CondGoal),
        translate_expected_product(Then, Expected, GsT, Tv),
        goals_list_to_conj(GsT, ConT),
        build_branch(ConT, Tv, Out, BT),
        ( ConC == true -> Goals = [(CondGoal -> BT)]
                        ; Goals = [(ConC, (CondGoal -> BT))] ).
translate_expected_product([if, Cond, Then, Else], Expected, Goals, Out) :- !,
        translate_if_cond(Cond, ConC, CondGoal),
        translate_expected_product(Then, Expected, GsT, Tv),
        translate_expected_product(Else, Expected, GsE, Ev),
        goals_list_to_conj(GsT, ConT),
        goals_list_to_conj(GsE, ConE),
        build_branch(ConT, Tv, Out, BT),
        build_branch(ConE, Ev, Out, BE),
        ( ConC == true -> Goals = [(CondGoal -> BT ; BE)]
                        ; Goals = [(ConC, (CondGoal -> BT ; BE))] ).
translate_expected_product([case, KeyExpr, Pairs], Expected, Goals, Out) :-
        is_list(Pairs),
        forall(member(Pair, Pairs), (is_list(Pair), length(Pair, 2))), !,
        ( select(Found0, Pairs, Rest0), subsumes_term(['Empty', _], Found0),
          Found0 = ['Empty', DefaultExpr]
          -> translate_expr_to_conj(KeyExpr, KeyConj, Kv),
             translate_case_expected(Rest0, Kv, Expected, Out, CaseGoal, KeyGoal),
             translate_expected_product(DefaultExpr, Expected, GsD, DOut),
             goals_list_to_conj(GsD, ConD),
             build_branch(ConD, DOut, Out, DefaultThen),
             Combined = ((KeyConj, CaseGoal) ; \+ KeyConj, DefaultThen),
             append(KeyGoal, [Combined], Goals)
           ; translate_expr(KeyExpr, Gk, Kv),
             translate_case_expected(Pairs, Kv, Expected, Out, IfGoal, KeyGoal),
             append([Gk, KeyGoal, [IfGoal]], Goals) ).
translate_expected_product([Kind, Pat, Val, In], Expected, Goals, Out) :-
        (Kind == let ; Kind == chain), !,
        translate_expr(Pat, Gp, Pv),
        translate_let_value(Pat, Val, In, Gv, V),
        note_candidates(Pv, V),
        bind_pattern_from(Pat, V),
        translate_expected_product(In, Expected, Gi, Out),
        append([[(Pv=V)], Gp, Gv, Gi], Goals).
translate_expected_product(['let*', Binds, Body], Expected, Goals, Out) :- !,
        letstar_to_rec_let(Binds, Body, RecLet),
        translate_expected_product(RecLet, Expected, Goals, Out).
translate_expected_product(Expr, _, Goals, Out) :- translate_expr(Expr, Goals, Out).

translate_case_expected(Pairs, Kv, Expected, Out, Goal, KGo) :-
        translate_case_expected(Pairs, Kv, Expected, Out, Goal, KGo, []).

translate_case_expected([[K,VExpr]|Rs], Kv, Expected, Out, Goal, KGo, Prior) :-
        ( var(Kv), known_singleton(Kv, KT), nonvar(KT)
          -> bind_pattern_typed(K, KT, Prior)
           ; ctor_pattern_field_types(K) ),
        translate_expected_product(VExpr, Expected, GsV, VOut),
        goals_list_to_conj(GsV, ConV),
        constrain_args(K, Kc, Gc),
        build_branch(ConV, VOut, Out, Then),
        ( Rs == [] -> Goal = ((Kv = Kc) -> Then), KGi = []
        ; translate_case_expected(Rs, Kv, Expected, Out, Next, KGi, [K|Prior]),
          Goal = ((Kv = Kc) -> Then ; Next) ),
        append([Gc, KGi], KGo).

%`data` is an erased, explicitly non-callable expression constructor. With an
%expected positional shape, every field is checked as a declared obligation
%before the result receives that type. Without an expectation, fully known
%field types still give the result a bottom-up positional type.
translate_explicit_data(Fields, expected(FieldTs), Goals, Out) :- !,
        translate_expected_fields(Fields, FieldTs, Gs, Values),
        set_out_type(Out, FieldTs),
        append(Gs, [Out = Values], Goals).
translate_explicit_data(Fields, none, Goals, Out) :-
        translate_args(Fields, Gs, Values),
        ( maplist(value_single_type, Values, FieldTs)
          -> set_out_type(Out, FieldTs) ; true ),
        append(Gs, [Out = Values], Goals).

%`make-list` is the erased, explicitly non-callable runtime-list constructor.
%An expected element type controls each element; Expression keeps its raw
%source term, while other nested products/lists recurse through expectations.
translate_explicit_list(Elements, expected(ET), Goals, Out) :- !,
        translate_expected_list_elements(Elements, ET, Gs, Values),
        set_out_type(Out, ['List', ET]),
        append(Gs, [Out = Values], Goals).
translate_explicit_list(Elements, none, Goals, Out) :-
        translate_args(Elements, Gs, Values),
        ( maplist(value_single_type, Values, [T|Ts]),
          forall(member(T2, Ts), T2 =@= T)
          -> set_out_type(Out, ['List', T]) ; true ),
        append(Gs, [Out = Values], Goals).

translate_expected_list_elements([], _, [], []).
translate_expected_list_elements([E|Es], ET, Goals, [V|Vs]) :-
        ( expression_typed(ET)
          -> expression_arg_value(E, V), G1 = []
        ; translate_expected_list_element(E, ET, 'make-list', G1, V) ),
        translate_expected_list_elements(Es, ET, G2, Vs),
        append(G1, G2, Goals).

translate_expected_list_element(E, T, Context, Goals, V) :-
        ( contextual_expected_type(T)
          -> translate_expected_product(E, T, G0, V)
           ; translate_expr(E, G0, V) ),
        check_call_arg(declared, Context, V, T, Checks),
        append(G0, Checks, Goals).

translate_expected_fields([], [], [], []).
translate_expected_fields([E|Es], [T|Ts], Goals, [V|Vs]) :-
        ( contextual_expected_type(T)
          -> translate_expected_product(E, T, G0, V),
             check_call_arg(declared, data, V, T, Checks),
             append(G0, Checks, G1)
           ; translate_expr(E, G0, V),
             check_call_arg(declared, data, V, T, Checks),
             append(G0, Checks, G1) ),
        translate_expected_fields(Es, Ts, G2, Vs),
        append(G1, G2, Goals).

%Special stream operation rewrite rules before main translation
rewrite_streamops(['trace!', Arg1, Arg2],
                  [progn, ['println!', Arg1], Arg2]).
rewrite_streamops([unique, Arg],
                  [call, [superpose, ['unique-atom', [collapse, Arg]]]]).
rewrite_streamops(['alpha-unique', Arg],
                  [call, [superpose, ['alpha-unique-atom', [collapse, Arg]]]]).
%Only the one-argument standard-library resolver is curated. Two-argument
%library paths can come from git-import!, and ordinary file imports remain
%user-origin.
rewrite_streamops(['import!', Space, [library, Name]],
                  ['library-import!', Space, Name]).
rewrite_streamops([union, [superpose|A], [superpose|B]],
                  [call, [superpose, ['union-atom', [collapse, [superpose|A]],
                                                    [collapse, [superpose|B]]]]]).
rewrite_streamops([intersection, [superpose|A], [superpose|B]],
                  [call, [superpose, ['intersection-atom', [collapse, [superpose|A]],
                                                           [collapse, [superpose|B]]]]]).
rewrite_streamops([subtraction, [superpose|A], [superpose|B]],
                  [call, [superpose, ['subtraction-atom', [collapse, [superpose|A]],
                                                          [collapse, [superpose|B]]]]]).
rewrite_streamops(X, X).

%Guarded stream ops rewrite rule application, successfully avoiding copy_term:
safe_rewrite_streamops(In, Out) :- ( compound(In), In = [Op|_], atom(Op) -> rewrite_streamops(In, Out)
                                                                          ; Out = In).

%Only literal, declared source-space names opt in. Raw space payloads and
%patterns are never evaluated here: reject a definite contradiction, trust
%unknown runtime-filled fields, and let the existing binder narrow unions.
typed_source_space(Space, RowT) :- atom(Space), declared_space_type(Space, RowT).

check_typed_space_value(Space, Value) :-
    ( typed_source_space(Space, RowT), value_definitely_mismatch(Value, RowT)
      -> throw(error(literal_type_mismatch(Value, RowT), typecheck))
    ; true ).

bind_typed_space_pattern(Space, Pattern) :-
    ( typed_source_space(Space, RowT)
      -> ( typed_space_pattern_mismatch(Pattern, RowT)
           -> throw(error(literal_type_mismatch(Pattern, RowT), typecheck))
         ; bind_pattern_typed(Pattern, RowT, []) )
    ; true ).

%Pattern-only wrappers are not literal row structure. Strip them (recursively,
%so annotated fields remain unknown) before asking the ordinary value checker
%whether the remaining tags, arities, and literals make a match impossible.
typed_space_pattern_mismatch(Pattern, RowT) :-
    pattern_value_shape(Pattern, Shape),
    value_definitely_mismatch(Shape, RowT).

pattern_value_shape(P, P) :- var(P), !.
pattern_value_shape([At, _Whole, Inner], Shape) :- At == '@', !,
                                                   pattern_value_shape(Inner, Shape).
pattern_value_shape([C, V, _Ty], V) :- C == (:), !.
pattern_value_shape(P, Shape) :- is_list(P), !, maplist(pattern_value_shape, P, Shape).
pattern_value_shape(P, P).

%The registry owns the mapping from builtin/arity to these procedural lowering
%hooks. Keeping this explicit list beside the implementations lets its
%load-time validator reject a misspelled or removed hook.
builtin_codegen_rule_defined(and_then).
builtin_codegen_rule_defined(arithmetic_native).
builtin_codegen_rule_defined(brand).
builtin_codegen_rule_defined(case).
builtin_codegen_rule_defined(catch).
builtin_codegen_rule_defined(collapse_all).
builtin_codegen_rule_defined(cut).
builtin_codegen_rule_defined(dynamic_reduce).
builtin_codegen_rule_defined(eval_source).
builtin_codegen_rule_defined(explicit_data).
builtin_codegen_rule_defined(explicit_list).
builtin_codegen_rule_defined(filter_pseudo_lambda).
builtin_codegen_rule_defined(foldall).
builtin_codegen_rule_defined(foldl_pseudo_lambda).
builtin_codegen_rule_defined(forall).
builtin_codegen_rule_defined(hyperpose).
builtin_codegen_rule_defined(if_then).
builtin_codegen_rule_defined(if_then_else).
builtin_codegen_rule_defined(lambda).
builtin_codegen_rule_defined(let_bind).
builtin_codegen_rule_defined(let_star).
builtin_codegen_rule_defined(manual_call).
builtin_codegen_rule_defined(map_pseudo_lambda).
builtin_codegen_rule_defined(once).
builtin_codegen_rule_defined(or_else).
builtin_codegen_rule_defined(progn).
builtin_codegen_rule_defined(prog1).
builtin_codegen_rule_defined(quote).
builtin_codegen_rule_defined(reified_comparison).
builtin_codegen_rule_defined(sealed).
builtin_codegen_rule_defined(superpose_literal).
builtin_codegen_rule_defined(test_collect).
builtin_codegen_rule_defined(transaction).
builtin_codegen_rule_defined(translate_predicate).
builtin_codegen_rule_defined(type_ascription).
builtin_codegen_rule_defined(typed_space_match).
builtin_codegen_rule_defined(typed_space_update).
builtin_codegen_rule_defined(with_mutex).

%Turn MeTTa code S-expression into goals list:
translate_expr(X, [], X)          :- ((var(X) ; atomic(X)) ; X = partial(_,_)), !.
translate_expr([H0|T0], Goals, Out) :-
        safe_rewrite_streamops([H0|T0],[H|T]),
        translate_expr(H, GsH, HV),
        %--- Translator rules ---:
        ( nonvar(HV), translator_rule(HV) -> length(T, NHook),
                                             ( once(fn_decl_arity(HV, NHook, ArgTypes, _))
                                               -> translate_args_by_type(T, ArgTypes, GsT, T1)
                                                ; translate_args(T, GsT, T1) ),
                                             append(T1,[Gs],Args),
                                             HookCall =.. [HV|Args],
                                             call(HookCall),
                                             translate_expr(Gs, GsE, Out),
                                             append([GsH,GsT,GsE],Goals)
        %--- Unambiguous expression-data construction. `data` is erased: its
        %arguments become the fields of the resulting expression, and its
        %first field is never interpreted as a function/closure to call.
        ; special_builtin_form(HV, T, explicit_data)
          -> translate_explicit_data(T, none, GsD, Out),
                        append(GsH, GsD, Goals)
        %--- Explicit runtime-list construction. Like `data`, `make-list`
        %erases and never dispatches its first element as a callable head.
        ; special_builtin_form(HV, T, explicit_list)
          -> translate_explicit_list(T, none, GsL, Out),
                               append(GsH, GsL, Goals)
        %--- Non-determinism ---:
        ; special_builtin_form(HV, T, superpose_literal),
          T = [Args], is_list(Args) -> build_superpose_branches(Args, Out, Branches),
                                                        disj_list(Branches, Disj),
                                                        append(GsH, [Disj], Goals)
        ; special_builtin_form(HV, T, collapse_all), T = [E]
          -> translate_expr_to_conj(E, Conj, EV),
                                     %always a list; a single element type is carried, several
                                     %become a union (an open variable would later unify with a
                                     %concrete requirement and wrongly certify a mixed list):
                                     %an element type that is not certain leaves ET open, which
                                     %would unify with ANY required element type: record the
                                     %ignorance explicitly so the (List T) claim is not discharged
                                     %by the open variable alone
                                     collapse_elem_type(EV, ET, ElemKnown),
                                     set_out_type(Out, ['List', ET]),
                                     ( ElemKnown == true -> true ; note_unknown_candidate(Out) ),
                                     append(GsH, [findall(EV, Conj, Out)], Goals)
        ; special_builtin_form(HV, T, cut), T = []
          -> append(GsH, [(!)], Goals),
                               Out = true
        ; special_builtin_form(HV, T, test_collect), T = [Expr, Expected]
          -> translate_expr_to_conj(Expr, Conj, Val),
                                              translate_expr(Expected, GsE, ExpVal),
                                              Goal1 = ( findall(Val, Conj, Results),
                                                        (Results = [Actual] -> true
                                                                             ; Actual = Results ) ),
                                              append(GsH, [Goal1], G1),
                                              append(G1, GsE, G2),
                                              append(G2, [test(Actual, ExpVal, Out)], Goals)
        ; special_builtin_form(HV, T, once), T = [X]
          -> translate_expr_to_conj(X, Conj, Out),
                                 append(GsH, [once(Conj)], Goals)
        ; special_builtin_form(HV, T, hyperpose), T = [L]
          -> ( nonvar(L), is_list(L)
               -> build_hyperpose_branches(L, Branches),
                  maplist({Out}/[(_,Res)]>>note_candidates(Out, Res), Branches),
                  append(GsH, [concurrent_and(member((Goal,Res), Branches), (call(Goal), Out = Res))], Goals)
               ; translate_expr(L, GsL, LV),
                 append(GsH, GsL, Inner),
                 append(Inner, [hyperpose_runtime(LV, Out)], Goals) )
        ; special_builtin_form(HV, T, with_mutex), T = [M,X]
          -> translate_expr_to_conj(X, Conj, Out),
                                         append(GsH, [with_mutex(M,Conj)], Goals)
        ; special_builtin_form(HV, T, transaction), T = [X]
          -> translate_expr_to_conj(X, Conj, Out),
                                        append(GsH, [transaction(Conj)], Goals)
        %--- Sequential execution ---:
        ; special_builtin_form(HV, T, progn), T = Exprs
          -> translate_args(Exprs, GsList, Outs),
                                    append(GsH, GsList, Tmp),
                                    last(Outs, Out),
                                    Goals = Tmp
        ; special_builtin_form(HV, T, prog1), T = Exprs
          -> Exprs = [First|Rest],
                                    translate_expr(First, GsF, Out),
                                    translate_args(Rest, GsRest, _),
                                    append(GsH, GsF, Tmp1),
                                    append(Tmp1, GsRest, Goals)
        %--- Conditionals ---:
        ; special_builtin_form(HV, T, if_then), T = [Cond, Then]
          -> translate_if_cond(Cond, ConC, CondGoal),
                                        translate_expr_to_conj(Then, ConT, Tv),
                                        build_branch(ConT, Tv, Out, BT),
                                        ( ConC == true -> append(GsH, [ ( CondGoal -> BT ) ], Goals)
                                                        ; append(GsH, [ ( ConC, ( CondGoal -> BT ) ) ], Goals) )
        ; special_builtin_form(HV, T, if_then_else), T = [Cond, Then, Else]
          -> translate_if_cond(Cond, ConC, CondGoal),
                                              translate_expr_to_conj(Then, ConT, Tv),
                                              translate_expr_to_conj(Else, ConE, Ev),
                                              build_branch(ConT, Tv, Out, BT),
                                              build_branch(ConE, Ev, Out, BE),
                                              ( ConC == true -> append(GsH, [ (CondGoal -> BT ; BE) ], Goals)
                                                              ; append(GsH, [ (ConC, (CondGoal -> BT ; BE)) ], Goals) )
        %A case whose arguments do not fit (case Key ((Pattern Value) ...))
        %compiles as a data list - warn, because that is rarely intended and
        %the downstream errors do not mention case at all:
        ; builtin_codegen_symbol(HV, case),
          \+ ( T = [_, Ps], is_list(Ps), forall(member(Pr, Ps), (is_list(Pr), length(Pr, 2))) )
          -> format(user_error, "Warning: (case ...) does not match (case Key ((Pattern Value) ...)) and compiles as plain data~n", []),
             eval_data_list([HV|T], GsD, Out),
             append(GsH, GsD, Goals)
        ; special_builtin_form(HV, T, case), T = [KeyExpr, PairsExpr]
          -> ( select(Found0, PairsExpr, Rest0),
                                                    subsumes_term(['Empty', _], Found0),
                                                    Found0 = ['Empty', DefaultExpr],
                                                    NormalCases = Rest0
                                                    -> translate_expr_to_conj(KeyExpr, GkConj, Kv),
                                                       translate_case(NormalCases, Kv, Out, CaseGoal, KeyGoal),
                                                       translate_expr_to_conj(DefaultExpr, ConD, DOut),
                                                       build_branch(ConD, DOut, Out, DefaultThen),
                                                       Combined = ( (GkConj, CaseGoal) ;
                                                                    \+ GkConj, DefaultThen ),
                                                       append([GsH, KeyGoal, [Combined]], Goals)
                                                     ; translate_expr(KeyExpr, Gk, Kv),
                                                       translate_case(PairsExpr, Kv, Out, IfGoal, KeyGoal),
                                                       append([GsH, Gk, KeyGoal, [IfGoal]], Goals) )
        %--- Short-circuit boolean operators ---:
        ; special_builtin_form(HV, T, and_then), T = [A, B]
          -> translate_expr_to_conj(A, ConjA, Av),
                                           translate_expr_to_conj(B, ConjB, Bv),
                                           check_call_arg(declared, 'and-then', Av, 'Bool', GsA),
                                           check_call_arg(declared, 'and-then', Bv, 'Bool', GsB),
                                           goals_list_to_conj(GsA, GA), goals_list_to_conj(GsB, GB),
                                           set_out_type(Out, 'Bool'),
                                           append(GsH, [(ConjA, GA, (Av == true -> (ConjB, GB, Out = Bv) ; Out = false))], Goals)
        ; special_builtin_form(HV, T, or_else), T = [A, B]
          -> translate_expr_to_conj(A, ConjA, Av),
                                          translate_expr_to_conj(B, ConjB, Bv),
                                          check_call_arg(declared, 'or-else', Av, 'Bool', GsA),
                                          check_call_arg(declared, 'or-else', Bv, 'Bool', GsB),
                                          goals_list_to_conj(GsA, GA), goals_list_to_conj(GsB, GB),
                                          set_out_type(Out, 'Bool'),
                                          append(GsH, [(ConjA, GA, (Av == true -> Out = true ; (ConjB, GB, Out = Bv)))], Goals)
        %--- Unification constructs ---:
        ; special_builtin_form(HV, T, let_bind), T = [Pat, Val, In]
          -> translate_expr(Pat, Gp, Pv),
                                                           translate_let_value(Pat, Val, In, Gv, V),
                                                           note_candidates(Pv, V),        %the bound variable gets the value's type
                                                           bind_pattern_from(Pat, V),     %destructuring patterns type their fields
                                                           translate_expr(In,  Gi, Out),
                                                           append([GsH,[(Pv=V)],Gp,Gv,Gi], Goals)
        ; special_builtin_form(HV, T, let_star), T = [Binds, Body]
          -> letstar_to_rec_let(Binds,Body,RecLet),
                                             translate_expr(RecLet,  Goals, Out)
        ; special_builtin_form(HV, T, sealed), T = [Vars, Expr]
          -> translate_expr_to_conj(Expr, Con, Val),
                                            note_candidates(Out, Val),
                                            Goals = [copy_term(Vars,[Con,Val],_,[Ncon,Out]),Ncon]
        %--- Iterating over non-deterministic generators without reification ---:
        ; special_builtin_form(HV, T, forall), T = [GF, TF]
          -> ( is_list(GF) -> GF = [GFH|GFA],
                              translate_expr(GFH, GsGFH, GFHV),
                              translate_args(GFA, GsGFA, GFAv),
                              append(GsGFH, GsGFA, GsGF),
                              GenList = [GFHV|GFAv]
                            ; translate_expr(GF, GsGF, GFHV),
                              GenList = [GFHV] ),
             translate_expr(TF, GsTF, TFHV),
             TestList = [TFHV, V],
             goals_list_to_conj(GsGF, GPre),
             GenGoal = (GPre, reduce(GenList, V)),
             append(GsH, GsTF, Tmp0),
             set_out_type(Out, 'Bool'),
             append(Tmp0, [( forall(GenGoal, ( reduce(TestList, Truth), Truth == true )) -> Out = true ; Out = false )], Goals)
        ; special_builtin_form(HV, T, foldall), T = [AF, GF, InitS]
          -> translate_expr_to_conj(InitS, ConjInit, Init),
             translate_expr(AF, GsAF, AFV),
             ( GF = [M|_], (M==match ; M==let ; M=='let*') -> LambdaGF = ['|->', [], GF],
                                                              translate_expr(LambdaGF, GsGF, GFHV),
                                                              GenList = [GFHV]
             ; is_list(GF) -> GF = [GFH|GFA],
                              translate_expr(GFH, GsGFH, GFHV),
                              translate_args(GFA, GsGFA, GFAv),
                              append(GsGFH, GsGFA, GsGF),
                              GenList = [GFHV|GFAv]
                            ; translate_expr(GF, GsGF, GFHV),
                              GenList = [GFHV] ),
             append(GsH, GsAF, Tmp1),
             append(Tmp1, GsGF, Tmp2),
             foldall_out_type(AFV, Init, Out),
             append(Tmp2, [ConjInit, foldall(agg_reduce(AFV, V), reduce(GenList, V), Init, Out)], Goals)
        %--- Higher-order functions with pseudo-lambdas and lambdas ---:
        ; special_builtin_form(HV, T, foldl_pseudo_lambda),
          T = [List, Init, AccVar, XVar, Body]
          -> translate_expr_to_conj(List, ConjList, L),
             translate_expr_to_conj(Init, ConjInit, InitV),
             note_list_elem_type(XVar, L),
             translate_expr_to_conj(Body, BodyConj, BG),
             exclude(==(true), [ConjList, ConjInit], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [foldl([XVar, AccVar, NewAcc]>>(BodyConj, ( number(BG) -> NewAcc is BG ; NewAcc = BG )), L, InitV, Out)], Goals)
        ; special_builtin_form(HV, T, map_pseudo_lambda),
          T = [List, XVar, Body]
          -> translate_expr_to_conj(List, ConjList, L),
             note_list_elem_type(XVar, L),
             translate_expr_to_conj(Body, BodyCallConj, BodyCall),
             ( value_single_type(BodyCall, BT) -> set_out_type(Out, ['List', BT]) ; true ),
             exclude(==(true), [ConjList], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [maplist([XVar, Y]>>(BodyCallConj, ( number(BodyCall) -> Y is BodyCall ; Y = BodyCall )), L, Out)], Goals)
        ; special_builtin_form(HV, T, filter_pseudo_lambda),
          T = [List, XVar, Cond]
          -> translate_expr_to_conj(List, ConjList, L),
             note_list_elem_type(XVar, L),
             translate_expr_to_conj(Cond, CondConj, CondGoal),
             exclude(==(true), [ConjList], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [include([XVar]>>(CondConj, CondGoal), L, Out)], Goals)
        ; special_builtin_form(HV, T, lambda), T = [Args, Body]
          -> next_lambda_name(F),
                                           % find free (non-argument) variables in Body
                                           term_variables(Body, AllVars),
                                           term_variables(Args, ArgVars),
                                           exclude({ArgVars}/[V]>>memberchk_eq(V, ArgVars), AllVars, FreeVars),
                                           append(FreeVars, Args, FullArgs),
                                           % compile clause with all bound + free vars
                                           translate_clause([=, [F|FullArgs], Body], Clause),
                                           register_fun(F),
                                           assertz(Clause),
                                           format(atom(Label), "metta lambda (~w)", [F]),
                                           maybe_print_compiled_clause(Label, ['|->', Args, Body], Clause),
                                           length(FullArgs, N),
                                           Arity is N + 1,
                                           (arity(F, Arity) -> true ; assertz(arity(F, Arity))),
                                           % emit closure capturing the environment (free vars)
                                           ( FreeVars == [] -> Out = F
                                                             ; Out = partial(F, FreeVars) )
        %--- Spaces ---:
        ; special_builtin_form(HV, T, typed_space_update), T = [Space, Atom] ->
                                                                   check_typed_space_value(Space, Atom),
                                                                   translate_expr(Space, G1, S),
                                                                   Goal =.. [HV,S,Atom,Out],
                                                                   set_out_type(Out, 'Bool'),
                                                                   append([GsH,G1,[Goal]], Goals)
        ; special_builtin_form(HV, T, typed_space_match),
          T = [Space, Pattern, Body] -> translate_expr(Space, G1, S),
                                                     type_match_pattern(Pattern),
                                                     bind_typed_space_pattern(Space, Pattern),
                                                     translate_expr(Body, GsB, Out),
                                                     append(G1, [match(S, Pattern, Out, Out)], G2),
                                                     append(G2, GsB, Goals)
        %--- Predicate to compiled goal ---:
        ; special_builtin_form(HV, T, translate_predicate), T = [Expr]
          -> Expr = [S|Args],
                                                  translate_args(Args, GsArgs, ArgsOut),
                                                  Goal =.. [S|ArgsOut],
                                                  append(GsH, GsArgs, Inner),
                                                  append(Inner, [Goal], Goals)
        %--- Manual dispatch options: ---
        %Generate a predicate call on compilation, translating Args for nesting:
        ; special_builtin_form(HV, T, manual_call), T = [Expr]
          -> Expr = [F|Args],
                                     translate_args(Args, GsArgs, ArgsOut),
                                     append(GsH, GsArgs, Inner),
                                     append(ArgsOut, [Out], CallArgs),
                                     Goal =.. [F|CallArgs],
                                     length(Args, NC),
                                     manual_dispatch_arg_checks(F, NC, ArgsOut, GuardGs),
                                     set_unique_decl_out(F, NC, Out),
                                     append(Inner, GuardGs, Inner1),
                                     append(Inner1, [Goal], Goals)
        %Produce a dynamic dispatch, translating Args for nesting:
        ; special_builtin_form(HV, T, dynamic_reduce), T = [Expr]
          -> ( var(Expr) -> translate_expr(Expr, GsH, ExprOut),
                                                     Goals = [reduce(ExprOut, Out)|GsH]
                                                   ; Expr = [F|Args],
                                                     translate_args(Args, GsArgs, ArgsOut),
                                                     append(GsH, GsArgs, Inner),
                                                     ExprOut = [F|ArgsOut],
                                                     length(Args, NR),
                                                     manual_dispatch_arg_checks(F, NR, ArgsOut, GuardGs),
                                                     set_unique_decl_out(F, NR, Out),
                                                     append(Inner, GuardGs, Inner1),
                                                     append(Inner1, [reduce(ExprOut, Out)], Goals) )
        %Invoke translator to evaluate MeTTa code as data/list:
        ; special_builtin_form(HV, T, eval_source), T = [Arg]
          -> ( nonvar(Arg), Arg = [Q, Quoted], Q == quote
                                     -> translate_expr(Quoted, GsQ, Out),           %(eval (quote E)) == E
                                        append(GsH, GsQ, Goals)
                                      ; append(GsH, [eval(Arg, Out)], Goals) )
        %Erased branding of a semantic role: knowledge only, no runtime goal:
        ; special_builtin_form(HV, T, brand), T = [TypeExpr, Expr]
          -> translate_expr(Expr, GsE, Out0),
                                               normalize_type(TypeExpr, TN),
                                               brand_type(Out0, TN),
                                               ( nonvar(Out0)               %a branded literal keeps its brand
                                                 -> add_known_type(Out, TN),
                                                    append([GsH, GsE, [Out = Out0]], Goals)
                                                  ; Out = Out0,
                                                    append(GsH, GsE, Goals) )
        %Explicit type ascription for dynamically typed values:
        ; special_builtin_form(HV, T, type_ascription),
          T = [TypeExpr, Expr] -> translate_expr(Expr, GsE, Out0),
                                             normalize_type(TypeExpr, TN),
                                             ascribe_type(Out0, TN, GsA),
                                             %An ascribed literal keeps the author's type when it is
                                             %more specific than the value's own (e.g. (the (List Item) ())):
                                             ( nonvar(Out0), nonvar(TN), \+ wildcard_type_t(TN),
                                               \+ ( value_single_type(Out0, VT), VT == TN )
                                               -> add_known_type(Out, TN),
                                                  append([GsH, GsE, GsA, [Out = Out0]], Goals)
                                                ; Out = Out0,
                                                  append([GsH, GsE, GsA], Goals) )
        %Force arg to remain data/list:
        ; special_builtin_form(HV, T, quote), T = [Expr]
          -> append(GsH, [], Inner),
                                     Out = Expr,
                                     Goals = Inner
        ; special_builtin_form(HV, T, catch), T = [Expr] ->
          translate_expr(Expr, GsExpr, ExprOut),
          append(GsH, [], Inner),
          goals_list_to_conj(GsExpr, Conj),
          Goal = catch((Conj, Out = ExprOut),
                       Exception,
                       (Exception = error(Type, Ctx) -> Out = ['Error', Type, Ctx]
                                                      ; Out = ['Error', Exception, none])),
          %The result is the value or an (Error Detail Ctx) expression: with a
          %known inner type that is a union, so union-typed consumers such as
          %(-> (| Number (Error $d $c)) ...) check statically:
          ( value_single_type(ExprOut, VT)
            -> set_out_type(Out, ['|', VT, ['Error', '%Undefined%', '%Undefined%']])
             ; true ),
          append(Inner, [Goal], Goals)
        %--- Automatic 'smart' dispatch, translator deciding when to create a predicate call, data list, or dynamic dispatch: ---
        %Known function or closure => type-directed call:
        ; ( atom(HV), fun(HV) -> Fun = HV, Bound = []
          ; compound(HV), HV = partial(Fun, Bound) )
          -> translate_typed_call(Fun, Bound, T, GsH, Goals, Out)
        %Literals (numbers, strings, etc.), known non-function atom => data:
        ; ( atomic(HV), \+ atom(HV) ; atom(HV), \+ fun(HV) )
          -> translate_args(T, GsT, AVs),
             append(GsH, GsT, Goals),
             Out = [HV|AVs]
        %Plain data list: evaluate inner fun-sublists
        ; is_list(HV) -> translate_args(T, GsT, AVs),
                         append(GsH, GsT, Inner),
                         eval_data_term(HV, Gd, HV1),
                         append(Inner, Gd, Goals),
                         Out = [HV1|AVs]
        %Unknown head (var/compound) => runtime dispatch, with a lean closure
        %application when the head's arrow type is known or assumed. A head
        %whose known type is provably not a function (e.g. Number) makes the
        %expression data construction, compiled and typed as such:
        ; translate_args(T, GsT, AVs),
          append(GsH, GsT, Inner),
          ( var(HV), AVs == []               %singleton ($x) is data, never an application
            -> Out = [HV],
               Goals = Inner
          ; translate_closure_call(HV, AVs, Inner, Goals, Out) -> true
          ; var(HV), known_singleton(HV, K), nonfunction_type(K)
            -> Out = [HV|AVs],
               Goals = Inner
          ; append(Inner, [reduce([HV|AVs], Out)], Goals) ) ).

%Values of these types can never dispatch as functions in reduce/2:
nonfunction_type(K) :- nonvar(K), ( primitive_type(K)
                                  ; is_list(K), \+ is_arrow_type(K) ).

%A variable head with a known arrow type of matching arity is a closure call:
%check the args against the arrow, dispatch through apply_fn (skipping reduce's
%per-call bookkeeping), and propagate the output type. Applying a parameter
%whose type is still an unbound assumption tells us it is a function.
translate_closure_call(HV, AVs, Inner, Goals, Out) :- var(HV), AVs \== [], known_singleton(HV, K),
                                                      length(AVs, N), N1 is N + 1,
                                                      %The arrow shape is a GUESS when K is unbound. On an
                                                      %inference assumption it is a good one and gets
                                                      %recorded; on a variable the declaration promised to
                                                      %callers it must not be, so the shape stays local and
                                                      %the result is honestly unknown (param_promise_var/1):
                                                      ( var(K) -> length(Xs, N1),
                                                                  ( param_promise_var(K) -> Guessed = true
                                                                                          ; K = [->|Xs], Guessed = false )
                                                      ; K = [H|Xs], arrow_atom(H),
                                                        length(Xs, N1), Guessed = false ),
                                                      append(ArgTs, [OutT], Xs),
                                                      apply_call_args(declared, closure, AVs, ArgTs, GuardGs),
                                                      append(Inner, GuardGs, Inner1),
                                                      closure_apply_goal(HV, AVs, Out, Goal),
                                                      append(Inner1, [Goal], Goals),
                                                      %a variable arrow output is knowledge too: it is the
                                                      %declaration-instance type var shared with the context
                                                      %(e.g. map-flat's element type), not an unknown:
                                                      ( Guessed == true -> note_unknown_candidate(Out)
                                                      ; var(Out), var(OutT) -> add_known_type(Out, OutT)
                                                                             ; set_out_type(Out, OutT) ).
%Underapplying a typed closure parameter builds a partial: the used argument
%positions are checked and the result carries the remaining arrow:
translate_closure_call(HV, AVs, Inner, Goals, Out) :- var(HV), AVs \== [], known_singleton(HV, K),
                                                      nonvar(K), K = [H|Xs],
                                                      arrow_atom(H),
                                                      length(AVs, N), length(Xs, LX), N < LX - 1,
                                                      append(ArgTs, [OutT], Xs),
                                                      length(UsedTs, N), append(UsedTs, RestTs, ArgTs),
                                                      apply_call_args(declared, closure, AVs, UsedTs, GuardGs),
                                                      append(Inner, GuardGs, Inner1),
                                                      append(Inner1, [reduce([HV|AVs], Out)], Goals),
                                                      append(RestTs, [OutT], RXs),
                                                      ( var(Out) -> add_known_type(Out, [H|RXs]) ; true ).

closure_apply_goal(HV, [A], Out, apply_fn1(HV, A, Out)) :- !.
closure_apply_goal(HV, [A, B], Out, apply_fn2(HV, A, B, Out)) :- !.
closure_apply_goal(HV, [A, B, C], Out, apply_fn3(HV, A, B, C, Out)) :- !.
closure_apply_goal(HV, AVs, Out, apply_fnN(HV, AVs, Out)).

%Runtime closure application; the last clause preserves reduce/2 semantics for
%values (including unbound heads used symbolically) that are not callable.
%A missing predicate (e.g. an arity the arrow type did not predict) fails like
%it always did - errors raised inside the callee propagate:
apply_fn1(F, A, Out) :- atom(F), fun(F), !, safe_apply(call(F, A, Out)).
apply_fn1(P, A, Out) :- compound(P), P = partial(F, Bs), !,
                        append(Bs, [A, Out], CallArgs),
                        Goal =.. [F|CallArgs], safe_apply(Goal).
apply_fn1(F, A, Out) :- reduce([F, A], Out).

apply_fn2(F, A, B, Out) :- atom(F), fun(F), !, safe_apply(call(F, A, B, Out)).
apply_fn2(P, A, B, Out) :- compound(P), P = partial(F, Bs), !,
                           append(Bs, [A, B, Out], CallArgs),
                           Goal =.. [F|CallArgs], safe_apply(Goal).
apply_fn2(F, A, B, Out) :- reduce([F, A, B], Out).

apply_fn3(F, A, B, C, Out) :- atom(F), fun(F), !, safe_apply(call(F, A, B, C, Out)).
apply_fn3(P, A, B, C, Out) :- compound(P), P = partial(F, Bs), !,
                              append(Bs, [A, B, C, Out], CallArgs),
                              Goal =.. [F|CallArgs], safe_apply(Goal).
apply_fn3(F, A, B, C, Out) :- reduce([F, A, B, C], Out).

apply_fnN(F, Args, Out) :- atom(F), fun(F), !, append(Args, [Out], CallArgs),
                           Goal =.. [F|CallArgs], safe_apply(Goal).
apply_fnN(P, Args, Out) :- compound(P), P = partial(F, Bs), !,
                           append(Bs, Args, All), append(All, [Out], CallArgs),
                           Goal =.. [F|CallArgs], safe_apply(Goal).
apply_fnN(F, Args, Out) :- reduce([F|Args], Out).

safe_apply(Goal) :- catch(Goal, error(existence_error(procedure, _), _), fail).

%Type-directed function call: check declared types at compile time, resolve
%overloads statically when possible, and emit runtime guards only where types
%stay unresolved (see AGENTS.md).
translate_typed_call(Fun, Bound, Args, GsH, Goals, Out) :-
        length(Args, NProv), length(Bound, NB), NTotal is NProv + NB,
        findall(ft(ATs, OT), fn_decl_arity(Fun, NTotal, ATs, OT), FullDecls),
        ( FullDecls \== []
          -> eff_arg_types(FullDecls, NB, NProv, EffTs),
             translate_typed_args(Fun, FullDecls, Args, EffTs, GsT, AVs0),
             append(Bound, AVs0, AVs),
             ( FullDecls = [Single] -> Chosen = Single, MultiDecl = false
             ; MultiDecl = true,
               include(decl_survives(AVs), FullDecls, Survivors),
               ( Survivors == [] -> throw(error(no_matching_overload(Fun), typecheck))
               ; Survivors = [OneLeft] -> Chosen = OneLeft
               ; Chosen = multi(Survivors) ) ),
             ( Chosen = ft(ATs, OT)
               -> apply_call_args(declared, Fun, AVs, ATs, GuardGs),
                  append([GsH, GsT, GuardGs], Inner),
                  %overloaded functions: clauses were not output-checked against a
                  %single declaration, so the call filters on the output type:
                  overload_out_guard(MultiDecl, Fun, Out, OT, Extra),
                  ( MultiDecl == false, arith_inline(Fun, AVs, Out, ArithGs)
                    -> append(Inner, ArithGs, Goals)
                     ; build_call_or_partial(Fun, AVs, Out, Inner, Extra, Goals) ),
                  set_call_out_type(Out, ATs, OT)
                ; Chosen = multi(Survs),
                  maplist(overload_branch(Fun, AVs, Out), Survs, Branches),
                  disj_list(Branches, Disj),
                  append(GsH, GsT, Pre),
                  append(Pre, [goal_or_throw(Disj, error(no_matching_overload(Fun), typecheck))], Goals) )
        ; findall(pt(PTs, RTs, OT), fn_decl_partial(Fun, NTotal, PTs, RTs, OT), PartDecls),
          PartDecls = [pt(PTs, _, _)]
          -> translate_args(Args, GsT, AVs0),                      %typed partial application
             append(Bound, AVs0, AVs),
             apply_call_args(declared, Fun, AVs, PTs, GuardGs),
             append([GsH, GsT, GuardGs], Inner),
             build_direct_call(Fun, AVs, Out, Inner, [], Goals)
        ; assumed_self_decl(Fun, NTotal, PTs, OutTv)
          -> translate_args(Args, GsT, AVs0),                      %self-recursion under the provisional type
             append(Bound, AVs0, AVs),                             %(before the store: earlier clauses' inference
             apply_call_args(inferred, Fun, AVs, PTs, GuardGs),    %is stale while later clauses widen it)
             append([GsH, GsT, GuardGs], Inner),
             build_call_or_partial(Fun, AVs, Out, Inner, [], Goals),
             ( var(Out) -> add_known_type(Out, OutTv) ; true )
        ; findall(it(IATs, IOT), inferred_decl_arity(Fun, NTotal, IATs, IOT), [it(IATs, IOT)])
          -> translate_args(Args, GsT, AVs0),                      %inferred type: knowledge only, never rejects
             append(Bound, AVs0, AVs),
             apply_call_args(inferred, Fun, AVs, IATs, GuardGs),
             append([GsH, GsT, GuardGs], Inner),
             build_call_or_partial(Fun, AVs, Out, Inner, [], Goals),
             set_out_type(Out, IOT)
        ; translate_args(Args, GsT, AVs0),                         %no type information
          append(Bound, AVs0, AVs),
          append(GsH, GsT, Inner),
          build_call_or_partial(Fun, AVs, Out, Inner, [], Goals),
          ( untyped_call_out(Fun, AVs, Out) -> true ; true ) ).

%Most calls retain ordinary bottom-up argument translation. Narrowly, at a
%uniquely declared call site, provided closure arguments first resolve shared
%type variables in that declaration. A remaining product/list argument can
%then receive contextual construction typing. This is closure-driven
%resolution, not general inference; the later apply_call_args call remains the
%single enforcement site for every argument. Goal order remains source order.
translate_typed_args(_, [ft(ATs, _)], Args, EffTs, Goals, Values) :-
        length(ATs, NDecl), length(Args, NProv), NB is NDecl - NProv, NB >= 0,
        length(BoundTs, NB), append(BoundTs, DeclTs, ATs),
        closure_contextual_positions(DeclTs), !,
        translate_declared_arrows(Args, EffTs, DeclTs, Staged),
        translate_contextual_args(Staged, GoalLists, Values),
        append(GoalLists, Goals).
translate_typed_args(_, _, Args, EffTs, Goals, Values) :-
        translate_args_by_type(Args, EffTs, Goals, Values).

closure_contextual_positions(Ts) :-
        nth0(ClosureI, Ts, ClosureT), is_arrow_type(ClosureT),
        nth0(ProductI, Ts, ProductT), ProductI =\= ClosureI,
        ( var(ProductT) ; contextual_expected_type(ProductT) ), !.

translate_declared_arrows([], [], [], []).
translate_declared_arrows([A|As], [ET|ETs], [DT|DTs], [S|Ss]) :-
        ( is_arrow_type(DT)
          -> translate_expr(A, G, V),
             ( argument_resolves_declared_type(V, DT) -> true ; true ),
             S = translated(G, V)
           ; S = pending(A, ET, DT) ),
        translate_declared_arrows(As, ETs, DTs, Ss).

argument_resolves_declared_type(V, DeclT) :-
        ( var(V) -> known_singleton(V, Actual)
                 ; value_single_type(V, Actual) ),
        type_unify(Actual, DeclT).

translate_contextual_args([], [], []).
translate_contextual_args([translated(G, V)|Ss], [G|Gs], [V|Vs]) :-
        translate_contextual_args(Ss, Gs, Vs).
translate_contextual_args([pending(A, ET, DT)|Ss], [G|Gs], [V|Vs]) :-
        ( contextual_expected_type(DT), \+ expression_typed(ET)
          -> translate_expected_product(A, DT, G, V)
        ; expression_typed(ET)
          -> expression_arg_value(A, V), G = []
           ; translate_expr(A, G, V) ),
        translate_contextual_args(Ss, Gs, Vs).

%A contextual data/list/cons producer is sometimes staged in an earlier let*
%binding. By the time its consumer call is translated, contextual translation
%of the binder is too late. Look ahead only through the remaining let/let*
%spine for any uniquely declared call using this exact binder. Resolve its
%other declared closure positions from source callable declarations, including
%partial applications, and translate the producer under the resulting
%expectation. This lookup uses its own fresh declaration copy.
translate_let_value(Pat, Val, In, Goals, V) :-
        nonvar(Val), Val = [Ctor|_],
        ( Ctor == data ; Ctor == 'make-list' ; Ctor == cons ),
        contextual_bound_initializer_type(Pat, In, Expected),
        contextual_expected_type(Expected), !,
        translate_expected_product(Val, Expected, Goals, V).
translate_let_value(_, Val, _, Goals, V) :- translate_expr(Val, Goals, V).

contextual_bound_initializer_type(Pat, In, Expected) :-
        var(Pat),
        contextual_initializer_use(In, Pat, Expected).

contextual_initializer_use(Expr, Binder, Expected) :-
        nonvar(Expr), Expr = [Kind, _Pat, Val, In],
        (Kind == let ; Kind == chain), !,
        ( contextual_initializer_use(Val, Binder, Expected)
        ; contextual_initializer_use(In, Binder, Expected) ).
contextual_initializer_use(Expr, Binder, Expected) :-
        nonvar(Expr), Expr = [Kind, Binds, Body], Kind == 'let*', !,
        letstar_to_rec_let(Binds, Body, RecLet),
        contextual_initializer_use(RecLet, Binder, Expected).
contextual_initializer_use(Expr, Binder, Expected) :-
        nonvar(Expr), Expr = [F|CallArgs], atom(F), is_list(CallArgs),
        length(CallArgs, N),
        findall(ft(ATs, OT), fn_decl_arity(F, N, ATs, OT), [ft(ATs, _)]),
        nth0(BinderI, CallArgs, Use), Use == Binder,
        nth0(BinderI, ATs, Expected),
        resolve_source_arrow_args(CallArgs, ATs, BinderI),
        contextual_expected_type(Expected), !.

resolve_source_arrow_args(Args, ATs, BinderI) :-
        resolve_source_arrow_args(Args, ATs, BinderI, 0).
resolve_source_arrow_args([], [], _, _).
resolve_source_arrow_args([A|As], [T|Ts], BinderI, I) :-
        ( I =\= BinderI, is_arrow_type(T)
          -> ( source_callable_arrow(A, Actual), type_unify(Actual, T)
               -> true ; true )
           ; true ),
        I1 is I + 1,
        resolve_source_arrow_args(As, Ts, BinderI, I1).

source_callable_arrow(G, Arrow) :-
        atom(G),
        findall(ft(ArgTs, OutT, H),
                ( declared_fn_type(G, ArgTs, OutT, Det),
                  length(ArgTs, N),
                  value_arrow_head(G, N, Det, H) ),
                [ft(ArgTs, OutT, H)]),
        append(ArgTs, [OutT], Tail),
        Arrow = [H|Tail].
source_callable_arrow(Source, Arrow) :-
        nonvar(Source), Source = [G|Bound], atom(G), is_list(Bound),
        length(Bound, N),
        findall(pt(RTs, OutT, H),
                ( fn_decl_partial(G, N, _PTs, RTs, OutT, Det),
                  length(RTs, NR), Total is N + NR,
                  value_arrow_head(G, Total, Det, H) ),
                [pt(RTs, OutT, H)]),
        append(RTs, [OutT], Tail),
        Arrow = [H|Tail].

%A provided arg position stays untranslated data iff every declaration types it
%Expression; the effective type feeds translate_args_by_type, which only ever
%distinguishes 'Expression' from everything else:
eff_arg_types(FullDecls, NB, NProv, Ts) :- NEnd is NB + NProv - 1,
                                           ( NProv =:= 0 -> Ts = []
                                           ; numlist(NB, NEnd, Is),
                                             maplist(eff_arg_type(FullDecls), Is, Ts) ).
eff_arg_type(FullDecls, I, T) :- ( forall(member(ft(ATs, _), FullDecls),
                                          ( nth0(I, ATs, Ty), expression_typed(Ty) ))
                                   -> T = 'Expression' ; true ).

%Expression-typed args stay unevaluated data, except underapplied callable
%expressions representable as a goal-free closure. Only expressions that can
%actually become a closure are translated, so plain data is never re-translated:
%brand is a checker construct, not data - it erases here too, branding the
%inner value as knowledge (use quote to pass a literal (brand ...) form):
expression_arg_value(A, AV) :- nonvar(A), A = [B, TypeExpr, Inner], B == brand, !,
                               expression_arg_value(Inner, AV),
                               normalize_type(TypeExpr, TN),
                               brand_type(AV, TN).
expression_arg_value(A, AV) :- ( maybe_closure_expr(A),
                                 catch(( translate_expr(A, GsExpr, AVExpr),
                                         trivial_goals(GsExpr),
                                         callable_expression_value(AVExpr) ),
                                       error(_, typecheck), fail)
                                 -> AV = AVExpr
                                  ; AV = A ).

%An underapplied call to a known function (would compile to partial(...)):
maybe_closure_expr([F|Args]) :- atom(F), fun(F), is_list(Args),
                                length(Args, N), Arity is N + 1,
                                \+ ( ( current_predicate(F/Arity) ; catch(arity(F, Arity), _, fail) ),
                                     \+ ( current_op(_, _, F), Arity =< 2 ) ).

trivial_goals([]).
trivial_goals([true|Gs]) :- trivial_goals(Gs).

callable_expression_value(AV) :- atom(AV), fun(AV).
callable_expression_value(partial(Fun, Bound)) :- atom(Fun), ground(Bound).

%One dispatch branch per surviving overload: non-throwing guards, then the call:
overload_branch(Fun, AVs, Out, ft(ATs, OT), Branch) :- maplist(overload_branch_guard(Fun), AVs, ATs, Gss),
                                                       append(Gss, GuardGs),
                                                       overload_out_guard(true, Fun, Out, OT, Extra),
                                                       build_direct_call(Fun, AVs, Out, GuardGs, Extra, BranchGoals),
                                                       goals_list_to_conj(BranchGoals, Branch).

overload_out_guard(MultiDecl, Fun, Out, OT, Extra) :- ( MultiDecl == true, ground(OT), \+ wildcard_type_t(OT)
                                                        -> ( strict_mode(true)
                                                             -> throw(error(strict_runtime_typecheck(Fun, typecheck_match(Out, OT)), typecheck))
                                                            ; trusted_library_decl(Fun)
                                                              -> Extra = []
                                                              ; Extra = [typecheck_match(Out, OT)] )
                                                         ; Extra = [] ).

overload_branch_guard(Fun, AV, T, G) :- ( arg_statically_ok(AV, T) -> G = []
                                        ; strict_mode(true)
                                          -> throw(error(strict_runtime_typecheck(Fun, typecheck_match(AV, T)), typecheck))
                                        ; trusted_library_decl(Fun)
                                          -> G = []
                                           ; G = [typecheck_match(AV, T)] ).

%Type-resolved builtin arithmetic compiles to native is/2, constant-folded when
%both operands are literals. Only while the builtin definition is untouched:
arith_inline(Fun, [A, B], Out, Gs) :- builtin_codegen_hook(Fun, 2, arithmetic_native),
                                      arith_op(Fun, A, B, Expr),
                                      builtin_untouched(Fun),
                                      ( number(A), number(B)
                                        -> catch((Out is Expr, Gs = []), _, Gs = [Out is Expr])
                                         ; Gs = [Out is Expr] ).

arith_op('+', A, B, A + B).
arith_op('-', A, B, A - B).
arith_op('*', A, B, A * B).
arith_op('/', A, B, A / B).
arith_op('%', A, B, A mod B).
arith_op(min, A, B, min(A, B)).
arith_op(max, A, B, max(A, B)).

builtin_untouched(F) :- functor(H, F, 3), predicate_property(H, number_of_clauses(1)).

%Reified comparisons whose result only feeds an if-condition compile to the
%native comparison, skipping the true/false round-trip:
translate_if_cond(Cond, PreConj, CondGoal) :- translate_expr(Cond, GsC, Cv),
                                              ( var(Cv), append(Pre, [Last], GsC), reified_cond(Last, Cv, Native)
                                                -> goals_list_to_conj(Pre, PreConj), CondGoal = Native
                                                 ; goals_list_to_conj(GsC, PreConj), CondGoal = (Cv == true) ).

reified_cond(G, Cv, Native) :- nonvar(G), G =.. [F, A, B, R], R == Cv,
                               builtin_codegen_hook(F, 2, reified_comparison),
                               cmp_native(F, A, B, Native),
                               builtin_untouched(F).

cmp_native('<', A, B, (A < B)).
cmp_native('<=', A, B, (A =< B)).
cmp_native('>', A, B, (A > B)).
cmp_native('>=', A, B, (A >= B)).
cmp_native('==', A, B, (A == B)).
cmp_native('!=', A, B, (A \== B)).

%Generate actual function call or partial if arity not complete:
build_call_or_partial(Fun, AVs, Out, Inner, Extra, Goals) :- ( maybe_specialize_call(Fun, AVs, Out, Goal)
                                                               -> oracle_det_wrap(Fun, AVs, Out, Goal, Goal1),
                                                                  append(Inner, [Goal1|Extra], Goals)
                                                                ; build_direct_call(Fun, AVs, Out, Inner, Extra, Goals) ).

build_direct_call(Fun, AVs, Out, Inner, Extra, Goals) :- length(AVs, N),
                                                         Arity is N + 1,
                                                         ( ( current_predicate(Fun/Arity) ; catch(arity(Fun, Arity), _, fail) ),
                                                           \+ ( current_op(_, _, Fun), Arity =< 2 )
                                                           -> append(AVs, [Out], CallArgs),
                                                              Goal0 =.. [Fun|CallArgs],
                                                              %--oracle-det: count this call's solutions
                                                              oracle_det_wrap(Fun, AVs, Out, Goal0, Goal),
                                                              append(Inner, [Goal|Extra], Goals)
                                                         ; incomplete_application_kind(Fun, Arity, partial)
                                                           -> Out = partial(Fun, AVs),
                                                              append(Inner, Extra, Goals)
                                                            ; append(Inner, [throw_function_overapplication(Fun, N)|Extra], Goals) ).

%Selectively apply translate_args for non-Expression args while Expression args stay as data input:
translate_args_by_type([], _, [], []) :- !.
translate_args_by_type([A|As], [T|Ts], GsOut, [AV|AVs]) :-
                      ( expression_typed(T) -> expression_arg_value(A, AV), GsA = []
                                           ; translate_expr(A, GsA, AV) ),
                      translate_args_by_type(As, Ts, GsRest, AVs),
                      append(GsA, GsRest, GsOut).

%Handle data list:
eval_data_term(X, [], X) :- (var(X); atomic(X)), !.
eval_data_term([F|As], Goals, Val) :- ( atom(F), fun(F) -> translate_expr([F|As], Goals, Val)
                                                         ; eval_data_list([F|As], Goals, Val) ).

%Handle data list entry:
eval_data_list([], [], []).
eval_data_list([E|Es], Goals, [V|Vs]) :- ( is_list(E) -> eval_data_term(E, G1, V) ; V = E, G1 = [] ),
                                         eval_data_list(Es, G2, Vs),
                                         append(G1, G2, Goals).


%Convert let* to recusrive let:
letstar_to_rec_let([[Pat,Val]],Body,[let,Pat,Val,Body]).
letstar_to_rec_let([[Pat,Val]|Rest],Body,[let,Pat,Val,Out]) :- letstar_to_rec_let(Rest,Body,Out).

%Patterns: variables, atoms, numbers, lists:
translate_pattern(X, X) :- var(X), !.
translate_pattern(X, X) :- atomic(X), !.
translate_pattern([H|T], [P|Ps]) :- !, translate_pattern(H, P),
                                       translate_pattern(T, Ps).

% Constructs the goal for a single branch of an if-then-else/case.
% A branch whose result variable carries no type knowledge is aliased straight
% onto the shared Out, which would otherwise let it inherit an EARLIER
% branch's candidates and vanish from the merge. Its ignorance is recorded
% first (see note_unknown_candidate/1) so the merged variable stays honest:
build_branch(true, Val, Out, (Out = Val)) :- !, note_candidates(Out, Val).
build_branch(Con, Val, Out, Goal) :- var(Val) -> ( known_candidates(Val, _) -> true
                                                                             ; Unknown = yes ),
                                                 Val = Out,
                                                 ( Unknown == yes -> note_unknown_candidate(Out)
                                                                   ; true ),
                                                 Goal = Con
                                               ; note_candidates(Out, Val),
                                                 Goal = (Val = Out, Con).

%Translate case expression recursively into nested if. The branches are
%compiled to a nested if-then-else, so case is first-match/COMMITTED: a value
%matched by an earlier branch never reaches a later one. Prior carries the
%earlier branches' patterns (source order) so the typechecker may use that
%exclusion when narrowing a union - see narrowing_sound/4.
translate_case(Pairs, Kv, Out, Goal, KGo) :- translate_case(Pairs, Kv, Out, Goal, KGo, []).

translate_case([[K,VExpr]|Rs], Kv, Out, Goal, KGo, Prior) :-
                                                      ( var(Kv), known_singleton(Kv, KT), nonvar(KT)
                                                        -> bind_pattern_typed(K, KT, Prior)
                                                         ; ctor_pattern_field_types(K) ),
                                                      translate_expr_to_conj(VExpr, ConV, VOut),
                                                      constrain_args(K, Kc, Gc),
                                                      build_branch(ConV, VOut, Out, Then),
                                                      ( Rs == [] -> Goal = ((Kv = Kc) -> Then), KGi=[]
                                                                  ; translate_case(Rs, Kv, Out, Next, KGi, [K|Prior]),
                                                                    Goal = ((Kv = Kc) -> Then ; Next) ),
                                                      append([Gc,KGi], KGo).

%Translate arguments recursively:
translate_args([], [], []).
translate_args([X|Xs], Goals, [V|Vs]) :- translate_expr(X, G1, V),
                                         translate_args(Xs, G2, Vs),
                                         append(G1, G2, Goals).

%foldall's result type is the accumulator function's output type when that is
%uniquely declared or inferred. The initial value's type is deliberately NOT
%used as a fallback: the result comes from the accumulator function, and the
%init only surfaces when the generator is empty.
%foldall returns Init when the generator is empty, so the accumulator's
%output type is only trustworthy when the initial value fits it too:
foldall_out_type(AFV, Init, Out) :- ( atom(AFV),
                                      findall(OT, ( fn_decl_arity(AFV, 2, _, OT)
                                                  ; inferred_decl_arity(AFV, 2, _, OT) ), [OT1]),
                                      ( var(Init) -> known_singleton(Init, IT)
                                      ; value_single_type(Init, IT) ),
                                      \+ \+ type_unify(IT, OT1)
                                      -> set_out_type(Out, OT1)
                                       ; true ).

%known_candidates_certain/2 refuses a candidate set containing the unknown
%marker: one alternative of undetermined type makes the whole element type
%unknown, and a (List T) claim over it would be exactly the certification the
%unknown alternative can break:
collapse_elem_type(EV, ET, Known) :- ( var(EV), known_candidates_certain(EV, Cs)
                                       -> ( Cs = [C1] -> ET = C1 ; ET = ['|'|Cs] ),
                                          Known = true
                                     ; nonvar(EV), value_single_type(EV, ET0) -> ET = ET0,
                                                                                 Known = true
                                     ; Known = false ).

%Build A ; B ; C ... from a list:
disj_list([G], G).
disj_list([G|Gs], (G ; R)) :- disj_list(Gs, R).

%Build one disjunct per branch: (Conj, Out = Val):
build_superpose_branches([], _, []).
build_superpose_branches([E|Es], Out, [B|Bs]) :- translate_expr_to_conj(E, Conj, Val),
                                                 build_branch(Conj, Val, Out, B),
                                                 build_superpose_branches(Es, Out, Bs).

%Build hyperpose branch as a goal list for concurrent_maplist to consume:
build_hyperpose_branches([], []).
build_hyperpose_branches([E|Es], [(Goal, Res)|Bs]) :- translate_expr_to_conj(E, Goal, Res),
                                                      build_hyperpose_branches(Es, Bs).

%Runtime hyperpose path for variable/computed list arguments.
hyperpose_runtime(Exprs, Out) :- is_list(Exprs),
                                 concurrent_and(member(Expr, Exprs), eval(Expr, Out)).

%Like membercheck but with direct equality rather than unification
memberchk_eq(V, [H|_]) :- V == H, !.
memberchk_eq(V, [_|T]) :- memberchk_eq(V, T).

%Generate readable lambda name:
next_lambda_name(Name) :- ( catch(nb_getval(lambda_counter, Prev), _, Prev = 0) ),
                          N is Prev + 1,
                          nb_setval(lambda_counter, N),
                          format(atom(Name), 'lambda_~d', [N]).

declared_output_type(F, OutType) :- atom(F),
									nonvar(OutType),
									catch(match('&self', [':', F, TypeChain], TypeChain, TypeChain), _, fail),
									TypeChain = [->|Types],
									append(_, [DeclaredOutType], Types),
									DeclaredOutType == OutType.

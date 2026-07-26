%%% Determinism arrows (-[det]->, -[semidet]->, -[nondet]->) %%%

%Several declarations at one arity are overloads, and the function as a whole
%can only be trusted with the WEAKEST commitment any of them makes: det and
%semidet both commit (semidet is the weaker of the two), while a nondet or an
%uncommitted plain -> overload leaves the whole function uncommitted. A
%det/nondet (or semidet/nondet) pair is a contradiction, not a weakening:
fn_determinism(F, N, Det) :- findall(D, ( declared_fn_type(F, ATs, _, D), length(ATs, N) ), Ds0),
                             sort(Ds0, Ds),
                             ( Ds == [] -> Det = unspecified
                             ; Ds = [D1] -> Det = D1
                             ; Ds == [det, semidet] -> Det = semidet
                             ; Ds == [det, unspecified] -> Det = unspecified
                             ; Ds == [semidet, unspecified] -> Det = unspecified
                             ; Ds == [nondet, unspecified] -> Det = nondet
                             ; throw(error(conflicting_determinism_declarations(F), determinism)) ).

validate_function_determinism(F, Args, BodyExpr, PrevClauses) :-
    length(Args, N),
    fn_determinism(F, N, Det),
    ( committed_det(Det) -> det_enforced_flag(F, N, Enf),
                            with_det_enforced(Enf,
                                with_det_head_vars(Args, ensure_deterministic_expr(Det, BodyExpr, F))),
                            ensure_non_overlapping_clause_heads(F, Args, PrevClauses)
                          ; true ).

%Publish the clause's HEAD variables for the duration of a body determinism
%analysis. A head variable is a parameter: it can arrive bound to anything the
%caller chose, functions included, whatever its declared type says - and a
%wildcard-typed parameter carries no type attribute at all (bind_param_type
%records nothing for Atom/%Undefined%), so identity is the only reliable test.
%unify_head_is_data/1 consults this to tell a parameter from a fresh local.
with_det_head_vars(Args, Goal) :- catch(b_getval('$det_head_vars', Saved), _, Saved = []),
                                  catch(b_getval('$det_direct_params', SavedD), _, SavedD = []),
                                  term_variables(Args, HVs),
                                  include(var, Args, DPs),
                                  setup_call_cleanup(( b_setval('$det_head_vars', HVs),
                                                       b_setval('$det_direct_params', DPs) ),
                                                     Goal,
                                                     ( b_setval('$det_head_vars', Saved),
                                                       b_setval('$det_direct_params', SavedD) )).

det_head_var(H) :- catch(b_getval('$det_head_vars', HVs), _, fail),
                   member(V, HVs), V == H, !.

%A DIRECT variable parameter: a top-level head argument that is ITSELF a
%variable, as opposed to a variable field inside a destructured parameter like
%(P $u). term_variables (det_head_var/1) cannot tell them apart - it flattens
%(P $u) to [$u] - but only direct params get the boundness check in
%translate_clause, so the strengthenings must key on THIS, not det_head_var/1.
det_direct_param(H) :- catch(b_getval('$det_direct_params', DPs), _, fail),
                       member(V, DPs), V == H, !.

%The commitment gate. Published alongside the head vars whenever a body's
%determinism is analysed; true only when the analysis subject itself carries an
%explicit -[det]->/-[semidet]-> arrow (validate_function_determinism), so its
%direct params are guaranteed bound at runtime by the boundary check. A
%transitive callee reached through body_determinism sets it from ITS OWN
%declaration, which in practice is never committed (a committed callee is
%answered from its declaration and never body-analysed), so it is false there -
%wired explicitly, not left to happen by accident.
with_det_enforced(Bool, Goal) :- catch(b_getval('$det_enforced', Saved), _, Saved = false),
                                 setup_call_cleanup(b_setval('$det_enforced', Bool),
                                                    Goal,
                                                    b_setval('$det_enforced', Saved)).

det_enforced_now :- catch(b_getval('$det_enforced', E), _, fail), E == true.

%A DIRECT parameter of a function under an explicit committed arrow: the
%boundary check guarantees it is bound at runtime, so - unlike an arbitrary
%typed argument, which may arrive unbound out of ordinary well-typed code - the
%strengthenings below may treat it as bound. A destructured FIELD is not a
%direct param and does NOT qualify (the boundary check is spine-level).
enforced_bound_param(V) :- det_direct_param(V), det_enforced_now.

%Whether the function whose body is about to be analysed carries an explicit
%committed arrow, as the boolean the gate publishes:
det_enforced_flag(F, N, Flag) :- ( explicit_committed_decl(F, N, _) -> Flag = true ; Flag = false ).

%A det body must neither branch nor fail; a semidet body is the same analysis
%minus the may-not-fail part - (empty) and calls to -[semidet]-> functions are
%exactly what it is allowed to do, and nothing else changes (superpose, match
%and overlapping heads stay rejected for both):
ensure_deterministic_expr(Det, Expr, Fun) :-
    deterministic_expr(Expr, R),
    ( R == ok -> true
    ; Det == semidet, R = may_fail(_) -> true
    ; R = nondeterministic(Reason) -> throw(error(determinism_conflict(Fun, Reason), determinism))
    ; R = may_fail(Reason) -> throw(error(determinism_conflict(Fun, Reason), determinism))
    ; throw(error(determinism_conflict(Fun, unknown(Expr)), determinism)) ).

%A clause whose body commits with (cut) never falls through to a later
%clause, so overlap with it cannot create a choicepoint:
ensure_non_overlapping_clause_heads(_, _, []).
ensure_non_overlapping_clause_heads(F, Args, [fun_meta(PrevArgs, PrevBody)|Rest]) :-
    ( clause_heads_overlap(Args, PrevArgs), \+ body_commits(PrevBody)
      -> throw(error(overlapping_deterministic_clauses(F, Args, PrevArgs), determinism))
       ; ensure_non_overlapping_clause_heads(F, Args, Rest) ).

%Only a cut guaranteed to execute before any failure or choice point makes
%the compiler's clause-entry commit equivalent to the source cut: the body
%must BE (cut), or bind it as the first let/let* value. A cut somewhere
%deeper (e.g. inside an if branch) may never run, so it exempts nothing.
body_commits(E) :- nonvar(E),
                   ( E = [C], C == cut -> true
                   ; E = [L, _, V, _], L == let -> nonvar(V), V = [C], C == cut
                   ; E = [L, [[_, V]|_], _], L == 'let*' -> nonvar(V), V = [C], C == cut
                   ; fail ).

clause_heads_overlap(ArgsA, ArgsB) :- copy_term((ArgsA, ArgsB), (CA, CB)),
                                      unifiable(CA, CB, _).


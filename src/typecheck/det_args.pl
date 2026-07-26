%%% Argument-aware determinism for builtins.
%
%builtin_call_determinism/3 is keyed on (name, arity) only, so one worst-case
%verdict has to cover every call site. Most of the weak verdicts above are
%weak for a SHAPE reason - the argument may be unbound, or an open list - and
%at a call site where the shape is manifest in the source the reason does not
%apply. This relation is consulted BEFORE the flat table and may only ever
%return a verdict at least as strong; where the shape cannot be established it
%simply fails and the flat table answers, the same provable-only discipline
%the -[det]-> exhaustiveness check uses.
%
%The judgements are about the SPINE, never the elements: a manifest list may
%hold unbound elements, and a builtin that raises on one (min_list/2 on a
%non-number) is still det, because an exception is not a solution.
call_site_determinism(F, N, Args, Det) :- builtin_call_determinism_args(F, N, Args, Det), !.
call_site_determinism(F, N, _, Det) :- function_call_determinism(F, N, Det).

%--- Strengthened by a manifest list SPINE.
%length/2, reverse/2, append/3 and friends invert over an open list; over a
%proper one they answer exactly once. The properness is read off the source.
builtin_call_determinism_args('size-atom', 1, [A], det) :- manifest_proper_list(A).
builtin_call_determinism_args(length, 1, [A], det) :- manifest_proper_list(A).
builtin_call_determinism_args(reverse, 1, [A], det) :- manifest_proper_list(A).
builtin_call_determinism_args('alpha-unique-atom', 1, [A], det) :- manifest_proper_list(A).
%append/3 and its aliases only need their FIRST list proper: the recursion is
%driven by it and the second operand is copied through untouched.
builtin_call_determinism_args('union-atom', 2, [A, _], det) :- manifest_proper_list(A).
builtin_call_determinism_args(append, 2, [A, _], det) :- manifest_proper_list(A).
%The multiset operations recurse on their first argument and commit to
%select/3's first solution with -> ; the second operand's shape is irrelevant.
builtin_call_determinism_args('subtraction-atom', 2, [A, _], det) :- manifest_proper_list(A).
builtin_call_determinism_args('intersection-atom', 2, [A, _], det) :- manifest_proper_list(A).
%exclude/3 walks its LIST argument, which is the second one here.
builtin_call_determinism_args('exclude-item', 2, [_, L], det) :- manifest_proper_list(L).
%last/2 and min/max_list/2 need the list NON-empty as well: they have no
%answer for (), and min-atom's non_list/1 guard does not catch it.
builtin_call_determinism_args(last, 1, [A], det) :- manifest_nonempty_list(A).
builtin_call_determinism_args('min-atom', 1, [A], det) :- manifest_nonempty_list(A).
builtin_call_determinism_args('max-atom', 1, [A], det) :- manifest_nonempty_list(A).
%nth0/3 enumerates only when the index is unbound; with a literal index it is
%semidet (out of range fails, and the range is not manifest).
builtin_call_determinism_args('index-atom', 2, [A, I], semidet) :- integer(I), manifest_proper_list(A).

%--- Strengthened by a manifest expression ARGUMENT.
%'add-atom'/3 and 'remove-atom'/3 (src/spaces.pl) are keyed semidet because
%their second clause reads Term as a list - add_sexp/remove_sexp do
%Term =.. [Space,Rel|Args], which FAILS on a non-list argument (an atom or an
%unbound variable), so a call whose argument shape is unknown may fail. When
%the argument is a manifest non-empty expression the =.. succeeds, assertz /
%retractall are det, and the function-equation first clause (guarded by cut on
%[=,_,_]) is equally det - so exactly one clause commits: the call is det.
%A DECLARED type never qualifies (see manifest_proper_list's note); only a
%literal spine built at the call site does.
builtin_call_determinism_args('add-atom', 2, [_, T], det) :- manifest_nonempty_list(T).
builtin_call_determinism_args('remove-atom', 2, [_, T], det) :- manifest_nonempty_list(T).
%The same reasoning, unlocked at a call site whose argument is not a manifest
%literal but a DIRECT parameter under an explicit committed arrow whose declared
%type is NOMINAL: every value of a nominal type is a constructor application, a
%nonempty list spine at runtime, so add_sexp/remove_sexp's =.. succeeds exactly
%once. The boundary check supplies the one fact the manifest-literal entries got
%from the source - that the argument is bound - which is why this is sound now
%and was not when the argument could arrive unbound out of well-typed code.
builtin_call_determinism_args('add-atom', 2, [_, T], det) :- enforced_bound_nominal(T).
builtin_call_determinism_args('remove-atom', 2, [_, T], det) :- enforced_bound_nominal(T).

%--- Strengthened by a bound probe against a ground, duplicate-free literal.
%is-member(X, L) is member(X, L) ; \+ member(X, L) - two mutually exclusive
%clauses. With X BOUND (an enforced-bound direct param, or a ground literal)
%and L a GROUND, DUPLICATE-FREE proper list, member/2 is a test that succeeds
%at most once, so exactly one of the two clauses yields exactly one solution:
%det. A duplicate in L would let clause one succeed twice, and an unbound X
%would let it enumerate - hence both conditions. The runtime predicate is left
%untouched: its generator mode is load-bearing (examples/functionhead3.metta).
builtin_call_determinism_args('is-member', 2, [Probe, L], det) :-
    is_member_probe_bound(Probe),
    manifest_ground_dupfree_list(L).

%--- Strengthened by manifestly bound boolean operands.
%and/2, or/2, not/1, xor/2 and implies/2 are nondet because bool/1 INVENTS a
%boolean it was not given. Where every operand is manifestly one already, that
%generator is a test and the if-then-else below it yields exactly one answer.
builtin_call_determinism_args(and, 2, Args, det) :- maplist(manifest_bool, Args).
builtin_call_determinism_args(or, 2, Args, det) :- maplist(manifest_bool, Args).
builtin_call_determinism_args(xor, 2, Args, det) :- maplist(manifest_bool, Args).
builtin_call_determinism_args(implies, 2, Args, det) :- maplist(manifest_bool, Args).
builtin_call_determinism_args(not, 1, Args, det) :- maplist(manifest_bool, Args).

%A source expression whose head is DATA rather than a function being applied.
%For a variable head this is the translator's own test (nonfunction_type/1 at
%translate_expr/3): anything else compiles to reduce/2 or apply_fn/3, i.e. an
%application whose result shape is not known here. An untyped variable head is
%therefore conservatively read as a call.
data_headed(H) :- var(H), !, known_singleton(H, K), nonfunction_type(K).
data_headed(H) :- atom(H), !, \+ fun(H).
%A COMPOUND head is itself an expression, and whether the whole thing is data
%follows the same rule one level down: ((c) 1 2) with c a declared constant is
%a nested data literal, but ((foo) 2 3) with foo a function is compiled as an
%application (apply_fn/reduce, translate_expr/3) whose result is whatever the
%applied closure returns - possibly (), possibly open - and no part of its
%spine is built at this call site. The old catch-all read every compound head
%as data, which strengthened min-atom to det over an application's result.
%A fun-headed compound is still a DATA head when the function's unique
%declared output type is a non-function type: ((identity-number $a) $b $c)
%evaluates its head to a Number, and a Number cannot dispatch - the
%translator compiles the spine as data for exactly that reason
%(nonfunction_type at translate_expr/3), so the manifest judgement asks the
%same question. A call returning an arrow (or a wildcard, which admits
%function symbols) stays an application: ((foo) 2 3) with foo returning a
%closure is exactly the case this predicate exists to exclude.
data_headed(H) :- is_list(H), H = [F|Fargs], !,
                  ( data_headed(F) -> true
                  ; atom(F), fun(F), length(Fargs, N),
                    findall(OT, fn_decl_arity(F, N, _, OT), [OT1]),
                    nonvar(OT1), nonfunction_type(OT1) ).
data_headed(_).

%Manifestly a proper list: the literal (), a literal expression whose head is
%data, or a cons onto a manifestly proper tail. In every case the SPINE is
%built by the compiler at the call site, which is what makes it a fact rather
%than a claim.
%
%A DECLARED type never qualifies, not even a fixed-width tuple like
%(Number Number). That looks like it fixes the spine and does not: the residual
%guard is typecheck_or_error/2, which succeeds on an unbound variable, so a
%(Number Number) parameter can arrive unbound out of ordinary well-typed code
%- (B $u) leaves its field unfilled - and min_list/2 on an unbound argument
%answers once and then raises. That is exactly the assumption commit 6996b5b
%removed from the flat table, and re-admitting it here would put it back.
manifest_proper_list(X) :- X == [], !.
manifest_proper_list(X) :- var(X), !, enforced_bound_tuple(X, _).
manifest_proper_list(X) :- is_list(X), X = [H|_], data_headed(H), !.
manifest_proper_list(X) :- nonvar(X), X = [C, _, Tl], ( C == cons ; C == 'cons-atom' ),
                           manifest_proper_list(Tl).

manifest_nonempty_list(X) :- var(X), !, enforced_bound_tuple(X, W), W >= 1.
manifest_nonempty_list(X) :- is_list(X), X = [H|_], data_headed(H), !.
manifest_nonempty_list(X) :- nonvar(X), X = [C, _, Tl], ( C == cons ; C == 'cons-atom' ),
                             manifest_proper_list(Tl).

%Manifestly a bound boolean: a literal, or a call to a det builtin whose only
%declared output type is Bool - such a call cannot answer with an unbound
%result, because the predicate builds true/false itself ('>'/3 is a single
%if-then-else over a comparison, and raises rather than guessing).
manifest_bool(X) :- X == true, !.
manifest_bool(X) :- X == false, !.
%A DIRECT parameter, under an explicit committed arrow, whose declared type is
%Bool: the boundary check makes it bound at runtime and its type makes it a
%boolean, so and/or/not/xor/implies find a value to test rather than a hole to
%enumerate. A Bool-typed destructured FIELD does NOT qualify - enforced_bound_param/1
%tests direct params only, and a field is skipped by the spine-level boundary check.
manifest_bool(X) :- var(X), !, enforced_bound_param(X), known_singleton(X, 'Bool').
manifest_bool([F|As]) :- atom(F), is_list(As), length(As, N),
                         builtin_call_determinism(F, N, det),
                         findall(OT, fn_decl_arity(F, N, _, OT), [OT1]), OT1 == 'Bool'.

%A bound is-member probe: a ground literal, or an enforced-bound direct param
%(any type - only boundness matters, since the probe is a test operand):
is_member_probe_bound(P) :- ground(P), !.
is_member_probe_bound(P) :- var(P), enforced_bound_param(P).

%A manifest proper list that is fully ground and duplicate-free. sort/2 dedups
%and orders; equal length to msort/2 (which keeps duplicates) means no dup:
manifest_ground_dupfree_list(L) :- manifest_proper_list(L), ground(L),
                                   sort(L, S), msort(L, M), length(S, K), length(M, K).

%A DIRECT parameter, under an explicit committed arrow, whose declared type is
%NOMINAL (a declared, non-wildcard, non-primitive atom type). Its values are
%constructor applications - nonempty list spines at runtime - so remove/add-atom
%is det. The boundary check removes the unbound-arrival case; the declaration
%makes the type nominal - PROVIDED no inhabitant is a bare atom: a nullary
%constructor or declared constant ((: left Left)) is an atom value, not a
%list spine, and remove_sexp's =.. FAILS on it - zero solutions under a det
%claim. The judgement reads the type's constructor set, so it is a snapshot
%like every other such verdict (note_ctor_snapshot/1): a constant declared in
%a later file recompiles this clause and withdraws the strengthening.
enforced_bound_nominal(T) :- var(T), enforced_bound_param(T),
                             known_singleton(T, K), atom(K),
                             user_atom_type(K), type_name_declared(K),
                             note_ctor_snapshot(K),
                             \+ nominal_nullary_inhabitant(K).

%An inhabitant of K that is a bare atom at runtime: a declared constant of
%type K, or a nullary constructor (-> K):
nominal_nullary_inhabitant(K) :- declared_value_type(C, K2), K2 == K, atom(C), \+ fun(C), !.
nominal_nullary_inhabitant(K) :- member_ctor(K, 0, _).

%A DIRECT parameter, under an explicit committed arrow, whose declared type is a
%fixed-width positional tuple (tagged_tuple_type/3's positional reading: a list
%of concrete types whose value is an N-element list, N>=1 - the tagged and
%untagged readings agree on width = length of the type list). The value is
%therefore a proper, nonempty list. This re-admits the (Number Number) min-atom
%case that 28e87bd removed: its ONLY objection was unbound arrival, and the
%boundary check removes exactly that, so the case is sound under enforcement.
enforced_bound_tuple(X, W) :- var(X), enforced_bound_param(X),
                              known_singleton(X, K),
                              is_list(K), \+ special_compound_type(K),
                              length(K, W), W >= 1.

%A deterministic caller needs positive evidence about its callees. Functions
%without a determinism arrow are analyzed from their translated clauses
%(bodies deterministic, heads non-overlapping), memoized, and treated as det
%on cycles (a recursive call cannot introduce what the rest disproves).
%A registered symbol with no MeTTa clauses is a Prolog builtin, and the only
%thing known about it is what builtin_call_determinism/3 records: everything
%else is `unspecified`. Assuming det for a predicate nobody analysed is the
%strongest claim available about the least visible code in the system, and it
%certified -[det]-> functions whose bodies backtrack (see (get-atoms ...)).
:- dynamic det_analysis_cache/3.

body_determinism(F, N, Det) :- det_analysis_cache(F, N, Det0), !, Det = Det0.
body_determinism(F, _, det) :- catch(b_getval('$det_stack', St), _, St = []),
                               memberchk(F, St), !.
body_determinism(F, N, Det) :- catch(nb_getval(F, Metas0), _, Metas0 = []),
                               include(arity_meta(N), Metas0, Metas),
                               ( Metas == []
                                 -> ( builtin_call_determinism(F, N, Det0)
                                      -> Det = Det0 ; Det = unspecified )
                                  ; catch(b_getval('$det_stack', St), _, St = []),
                                    b_setval('$det_stack', [F|St]),
                                    type_meta_params(F, N, Metas, Metas1),
                                    det_enforced_flag(F, N, Enf),
                                    with_det_enforced(Enf, clause_set_determinism(Metas1, Det0)),
                                    b_setval('$det_stack', St),
                                    Det = Det0,
                                    assertz(det_analysis_cache(F, N, Det)) ).

%A stored clause meta is captured before clause_param_types binds the
%declared arg types onto the head param vars, so those vars carry no type
%attribute. The unconditional own-body check (validate_function_determinism)
%analyzes the ACTUAL body whose vars ARE typed, but a transitive analysis of
%a callee reads only its untyped stored metas - and a data parameter then
%looks like a function of unknown determinism. In particular a var-headed
%tuple ($x ...) built from a data parameter is misread as a dynamic call
%(unknown) instead of deterministic data construction, so pure list/record
%helpers analyze as unspecified and wrongly poison any -[det]-> caller.
%Attach the declared parameter types to a COPY of each meta (never the stored
%one), mirroring clause_param_types, so the transitive analysis agrees with
%the direct one. Arrow parameters keep their declared arrow - a plain -> stays
%unspecified in every mode but --strict-det, exactly as before:
type_meta_params(F, N, Metas, Metas1) :- ( findall(ATs, fn_decl_arity(F, N, ATs, _), [ATs1])
                                           -> maplist(type_one_meta(ATs1), Metas, Metas1)
                                            ; Metas1 = Metas ).

type_one_meta(ATs1, Meta, Meta2) :- copy_term(Meta, Meta2),
                                    Meta2 = fun_meta(Args, _),
                                    maplist(bind_meta_param, Args, ATs1).

bind_meta_param(Arg, T) :- ignore(catch(bind_pattern_typed(Arg, T), _, true)).

arity_meta(N, fun_meta(Args, _)) :- length(Args, N).

%The worst verdict over ALL clause bodies decides (a may_fail clause followed
%by a nondeterministic one is nondet, not semidet), and overlapping heads
%multiply results whatever the bodies say:
clause_set_determinism(Metas, Det) :- clause_bodies_determinism(Metas, R),
                                      ( R = nondeterministic(_) -> Det = nondet
                                      ; R = unknown(_) -> Det = unspecified
                                      ; overlapping_meta_pair(Metas) -> Det = nondet
                                      ; R = may_fail(_) -> Det = semidet
                                      ; Det = det ).

%The metas are walked directly (never through findall) so the parameter type
%attributes type_meta_params/4 attached to the head vars stay visible in the
%bodies they are shared with:
clause_bodies_determinism([], ok).
%Each meta's Args are published as the head-variable set for its body's
%analysis (see with_det_head_vars/2): a wildcard-typed parameter has no type
%attribute, so identity against the head is what tells it from a fresh local.
clause_bodies_determinism([fun_meta(Args, B)|Ms], R) :-
                                                     with_det_head_vars(Args, B, deterministic_expr(B, R1)),
                                                     ( det_result_final(R1) -> R = R1
                                                     ; clause_bodies_determinism(Ms, R2),
                                                       combine_det_results(R1, R2, R) ).

overlapping_meta_pair(Metas) :- append(_, [fun_meta(A1, _)|Rest], Metas),
                                member(fun_meta(A2, B2), Rest),
                                clause_heads_overlap(A1, A2),
                                \+ body_commits(B2).

deterministic_expr(Expr, ok) :- ( var(Expr) ; atomic(Expr) ; Expr = partial(_, _) ), !.
%A variable head must not unify with the construct patterns below. An
%explicit -[det]-> arrow (or nonfunction data type) on the head is det
%evidence in every mode; a plain -> counts only under --strict-det, where
%it is a commitment:
deterministic_expr([Head|Args], Result) :- var(Head), !,
    ( Args == [] -> Result = ok                    %singleton ($x) is data, not application
    ; known_singleton(Head, K), nonvar(K)
      -> ( arrow_head_level(K, det) -> combine_determinism_list(Args, Result)
         ; arrow_head_level(K, plain), strict_det(true) -> combine_determinism_list(Args, Result)
         ; arrow_head_level(K, semidet)
           -> combine_determinism_list(Args, R0),
              combine_det_results(may_fail(semidet_closure), R0, Result)
         ; arrow_head_level(K, nondet) -> Result = nondeterministic(nondet_closure)
         %non-arrow head: data construction - but NOT through a wildcard.
         %Atom/%Undefined%/Expression admit function symbols, and a var of
         %such a type bound to one at runtime makes reduce/2 DISPATCH the
         %"data" this analysis said it was building:
         ; \+ is_arrow_type(K), \+ wildcard_type_t(K) -> combine_determinism_list(Args, Result)
         ; Result = unknown(dynamic_head(Head)) )
       ; Result = unknown(dynamic_head(Head)) ).
deterministic_expr([collapse, _], ok) :- !.
deterministic_expr(['trace!', A, B], Result) :- !, combine_determinism_list([A, B], Result).
deterministic_expr([once, Expr], Result) :- !, once_determinism(Expr, Result).
deterministic_expr([quote, _], ok) :- !.
deterministic_expr([eval, _], unknown(dynamic_eval)) :- !.
deterministic_expr([reduce, _], unknown(dynamic_reduce)) :- !.
deterministic_expr([call, Expr], Result) :- !, deterministic_call_expr(Expr, Result).
deterministic_expr([superpose|_], nondeterministic(superpose)) :- !.
deterministic_expr([match|_], nondeterministic(match)) :- !.
deterministic_expr([hyperpose|_], nondeterministic(hyperpose)) :- !.
deterministic_expr([translatePredicate|_], nondeterministic(translatePredicate)) :- !.
%The structural (dis)equality tests unify their operands as DATA. A var-headed
%operand like ($name $index $vars) is a PATTERN built and unified, never
%dispatched: reduce/2 leaves a var-headed term unevaluated (src/translator.pl),
%so it contributes no dynamic-head uncertainty and the whole test stays det.
%A FUN-headed operand IS evaluated first and still contributes its determinism,
%so the reading only differs from the generic call path at a variable head -
%exactly the pattern case the honest dynamic_head verdict is too weak for.
deterministic_expr([Op, A, B], Result) :- unify_test_op(Op), !,
                                          unify_operand_determinism(A, RA),
                                          unify_operand_determinism(B, RB),
                                          combine_det_results(RA, RB, Result).
%A two-argument if has no else branch: when the condition is false the whole
%expression produces NOTHING. That is a failure path of the construct itself,
%invisible to an analysis of the parts, so it is may_fail unconditionally -
%-[semidet]-> accepts it, -[det]-> does not:
deterministic_expr([if, Cond, Then], Result) :- !, combine_determinism_list([Cond, Then], R0),
                                                combine_det_results(may_fail(if_without_else), R0, Result).
deterministic_expr([if, Cond, Then, Else], Result) :- !, combine_determinism_list([Cond, Then, Else], Result).
deterministic_expr([progn|Exprs], Result) :- !, combine_determinism_list(Exprs, Result).
deterministic_expr([prog1|Exprs], Result) :- !, combine_determinism_list(Exprs, Result).
deterministic_expr([let, Pat, Val, In], Result) :- !, let_determinism(Pat, Val, In, Result).
deterministic_expr([chain, Pat, Val, In], Result) :- !, let_determinism(Pat, Val, In, Result).
deterministic_expr(['let*', Binds, Body], Result) :- !, binds_and_body_determinism(Binds, Body, Result).
deterministic_expr([sealed, _, Expr], Result) :- !, deterministic_expr(Expr, Result).
deterministic_expr(['forall', _, _], ok) :- !.
deterministic_expr(['foldall', _, _, _], ok) :- !.
%The three higher-order builtins exist in TWO forms, and both are live.
%
%  * The pseudo-lambda form the translator rewrites inline (src/translator.pl):
%    (foldl-atom List Init $acc $x Body), (map-atom List $x Body),
%    (filter-atom List $x Cond). The element variable is a binder, so the
%    determinism is that of the list and of the inlined body.
%  * The CLOSURE form, which is what src/metta.pl actually defines
%    ('map-atom'/3, 'foldl-atom'/4, 'filter-atom'/3 = 2/3/2 MeTTa arguments
%    plus the result): (map-atom List F), (foldl-atom List Init F),
%    (filter-atom List F). Here the determinism is the CLOSURE argument's -
%    the predicate calls reduce/2 on it once per element - so the closure has
%    to carry det evidence exactly as it does for a user-written fold
%    (det_arg_evidence/2, the caller-side discharge used by
%    det_closure_args_ok/3).
%
%Only the first form had clauses, so every closure-form call fell through to
%deterministic_call_expr/2, where the three are deliberately unlisted in
%builtin_call_determinism/3 ("their determinism is that of the closure they
%are given, which this table cannot express") and therefore `unspecified`.
%That is what rejected lib_he's for-each-in-atom under --strict-det.
%
%LIMIT, shared with the pseudo-lambda clauses above and stated rather than
%hidden: all six read the list argument as a PROPER list. It need not be one
%- 'map-atom'([], _, []) and 'map-atom'([H|T], ...) both match an unbound
%argument, so an open list enumerates lengths - but the closure's own
%determinism is the question these constructs are asked, and requiring the
%spine to be manifest would make (map-atom $l $f) unprovable for every
%function that takes its list as a parameter, i.e. all of them.
deterministic_expr(['foldl-atom', List, Init, _, _, Body], Result) :- !, combine_determinism_list([List, Init, Body], Result).
deterministic_expr(['map-atom', List, _, Body], Result) :- !, combine_determinism_list([List, Body], Result).
deterministic_expr(['filter-atom', List, _, Cond], Result) :- !, combine_determinism_list([List, Cond], Result).
deterministic_expr(['foldl-atom', List, Init, F], Result) :- !,
    closure_builtin_determinism('foldl-atom', F, 2, [List, Init], Result).
deterministic_expr(['map-atom', List, F], Result) :- !,
    closure_builtin_determinism('map-atom', F, 1, [List], Result).
deterministic_expr(['filter-atom', List, F], Result) :- !,
    closure_builtin_determinism('filter-atom', F, 1, [List], Result).
deterministic_expr(['|->', _, _], ok) :- !.
deterministic_expr([case, KeyExpr, PairsExpr], Result) :- !, case_expr_determinism(KeyExpr, PairsExpr, Result).
deterministic_expr([Head|Args], Result) :- ( atomic(Head), ( \+ atom(Head) ; \+ fun(Head) )
                                           ; is_list(Head) ), !,
                                           combine_determinism_list([Head|Args], Result).
deterministic_expr([Head|Args], Result) :- atom(Head), !, deterministic_call_expr([Head|Args], Result).
deterministic_expr([Head|_], unknown(dynamic_head(Head))).

%(map-atom L F), (foldl-atom L Init F) and (filter-atom L F): the data
%arguments contribute their own determinism, and the closure has to prove
%itself exactly as it does at a user-written higher-order call site.
closure_builtin_determinism(Name, F, M, DataArgs, Result) :-
    ( det_arg_evidence(F, M) -> combine_determinism_list(DataArgs, Result)
                              ; Result = unknown(undetermined_closure(Name, F)) ).

deterministic_call_expr([Fun|Args], Result) :- atom(Fun), !,
                                               length(Args, N),
                                               call_site_determinism(Fun, N, Args, Det),
                                               ( Det == nondet -> Result = nondeterministic(call(Fun))
                                               ; Det == semidet
                                                 -> combine_determinism_list(Args, R0),
                                                    combine_det_results(may_fail(call(Fun)), R0, Result)
                                               ; Det == det -> combine_determinism_list(Args, Result)
                                               ; det_closure_args_ok(Fun, N, Args), body_determinism_assuming(Fun, N, det)
                                                 -> combine_determinism_list(Args, Result)
                                               ; underapplied_closure(Fun, N) -> combine_determinism_list(Args, Result)
                                               ; Result = unknown(undetermined_call(Fun)) ).
deterministic_call_expr(Expr, unknown(dynamic_call(Expr))).

%Structural equality/unification builtins whose operands are DATA PATTERNS.
%These take their arguments unevaluated-as-data (a var head is unified, not
%dispatched), so a var-headed operand does not make the test unknown.
unify_test_op('=').
unify_test_op('=?').
unify_test_op('=alpha').
unify_test_op('=@=').

%Determinism of one operand of a structural test. A var-headed compound is a
%unification pattern: its head is data (never dispatched) and its arguments are
%themselves operands, so the pattern is det unless a nested fun-headed sub-call
%is not. A fun-headed (or atomic/var) operand is read exactly as anywhere else.
unify_operand_determinism(E, ok) :- ( var(E) ; atomic(E) ), !.
unify_operand_determinism([H|Args], R) :- var(H), unify_head_is_data(H), !,
                                          combine_unify_operands(Args, R).
unify_operand_determinism(E, R) :- deterministic_expr(E, R).

%A var head is data only when it provably CANNOT be a function at the reduce
%that builds the term. Two proofs exist: its known type rules functions out
%(non-arrow and non-wildcard - Atom/%Undefined%/Expression admit function
%symbols), or it is a fresh local: not a HEAD variable of the clause under
%analysis (det_head_var/1 - identity, because a wildcard-typed parameter
%carries no type attribute at all) and carrying no knowledge of any kind (a
%let/chain field is marked by let_determinism/4 exactly so it fails this
%test). A fresh local is bound by nothing when reduce/2 reaches it, and an
%unevaluated var-headed term is one solution of data. Without this test,
%(= 1 ($f 0)) with $f an Atom-typed parameter read as data while the compiled
%reduce dispatched whatever function the caller passed.
unify_head_is_data(H) :- ( known_singleton(H, K), nonvar(K)
                           -> \+ is_arrow_type(K), \+ wildcard_type_t(K)
                         ; det_head_var(H) -> fail
                         ; \+ get_attr(H, tknown, _) ).

combine_unify_operands([], ok).
combine_unify_operands([A|As], R) :- unify_operand_determinism(A, R1),
                                     combine_unify_operands(As, R2),
                                     combine_det_results(R1, R2, R).


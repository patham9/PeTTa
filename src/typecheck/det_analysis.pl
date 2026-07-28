%%% Argument-aware transitive determinism through higher-order functions.
%A function whose declared arrow is a plain -> (no det commitment outside
%--strict-det) can still be deterministic CONDITIONALLY on its closure
%arguments: fold-flat is det exactly when the folded closure is det. The
%unconditional body_determinism analyzes the callee's clauses in isolation -
%the closure param applies with a plain -> head, which is not det evidence -
%so it reports unspecified and a det caller's validation fails. Here, when
%the ACTUAL argument at a call site is det, we re-analyze the callee's
%clauses with the closure-parameter positions treated as det and certify the
%call. The det assumption is scoped to copies of the callee's clause metas;
%no global flag ever makes a plain -> arrow count as det.
%
%NOTE ON MECHANISM: the stored clause metas are captured (translator.pl)
%before clause_param_types binds the declared arrow types onto the head param
%vars, so those vars carry NO tknown arrow attribute. Rather than read and
%upgrade an existing attribute, we DERIVE the det arrow from the function's
%unique declaration and attach it to the copied head var at each arrow
%position. The net effect is identical - the copy's param var reads as
%-[det]-> for the var-head application in deterministic_expr - and it stays
%scoped to the copy; the stored metas are never mutated.
:- dynamic det_assume_cache/3.

%Declared arrow parameter positions (any head but -[nondet]->, whose det-ness
%is irrelevant because it is already handled as nondet), as
%pos(Index, InArity, Head) where InArity is the arrow's parameter count. A
%-[semidet]-> position is listed too: it is only ever UPGRADED to det when the
%actual argument proves det, and a semidet actual simply fails the evidence
%test below, which drops the whole path back to the normal fallbacks:
arrow_det_positions(ATs, Positions) :- findall(pos(Idx, M, H),
                                              ( nth0(Idx, ATs, T), is_arrow_type(T),
                                                T = [H|Rest], arrow_atom_det(H, L), L \== nondet,
                                                length(Rest, Len), M is Len - 1 ),
                                              Positions).

%Fun must have a UNIQUE arity-N declaration exposing at least one non-nondet
%arrow parameter, and every such position's actual argument must be det (else
%this path adds nothing and fails so the normal fallbacks run):
det_closure_args_ok(Fun, N, Args) :- findall(ATs, fn_decl_arity(Fun, N, ATs, _), [ATs1]),
                                     arrow_det_positions(ATs1, Positions),
                                     Positions \== [],
                                     det_closure_positions(Positions, Args).

det_closure_positions([], _).
det_closure_positions([pos(Idx, M, _)|Ps], Args) :- nth0(Idx, Args, Arg),
                                                    det_arg_evidence(Arg, M),
                                                    det_closure_positions(Ps, Args).

%An actual argument carries det evidence when it is: a var whose known arrow
%commits to det; a lambda with a det body; a (partial) application or bare
%atom naming a function that is det at the relevant arity. Anything else
%fails:
det_arg_evidence(Arg, _) :- var(Arg), !, known_singleton(Arg, K),
                            arrow_head_level(K, L),
                            ( L == det -> true ; L == plain, strict_det(true) ).
det_arg_evidence(['|->', _, LBody], _) :- !, deterministic_expr(LBody, ok).
det_arg_evidence([F2|_], _) :- atom(F2), !, fn_own_arity(F2, A), det_atom_evidence(F2, A).
det_arg_evidence(F2, M) :- atom(F2), !, det_atom_evidence(F2, M).

%A named function used as a VALUE is the same function it is when called, so
%it is judged by the same relation - builtin table first, then the declared
%arrow, then clause analysis. Reading only the declaration here is what let
%(: or (-> Bool Bool Bool)) certify (fold-flat or False ...) as det under
%--strict-det, where plain_arrow_det/1 turns a plain -> into a commitment,
%while a direct call to or/2 in the same body was rejected: one symbol, two
%verdicts. The table is the checker's own knowledge and outranks a
%declaration it contradicts, exactly as it does for a direct call and for the
%oracle's wrapping decision (oracle_det_believed/3).
det_atom_evidence(F2, M) :- catch(function_call_determinism(F2, M, Det), _, fail), Det == det.

%The named function's own full arity (declared, else from stored clauses):
fn_own_arity(F2, A) :- fn_decl_arity(F2, A, _, _), !.
fn_own_arity(F2, A) :- catch(nb_getval(F2, Metas), _, fail), member(fun_meta(As, _), Metas), length(As, A), !.

%body_determinism GIVEN the arrow-typed parameters are det. Analyzes COPIES
%of the stored clause metas with each arrow-position head param var attached
%the -[det]-> form of its declared arrow type; the stored metas stay intact.
%A SEPARATE assume-stack breaks recursion (the callee re-enters this path
%with the now-det closure var, and det_closure_args_ok re-confirms it) and a
%separate cache memoizes the argument-independent ("det GIVEN det closures")
%result. Never reuses body_determinism's $det_stack - that would let the
%unconditional analysis wrongly certify a plain -> as det.
body_determinism_assuming(F, N, Det) :- det_assume_cache(F, N, Det0), !, Det = Det0.
body_determinism_assuming(F, _, det) :- catch(b_getval('$det_assume_stack', St), _, St = []),
                                        memberchk(F, St), !.
body_determinism_assuming(F, N, Det) :- catch(nb_getval(F, Metas0), _, Metas0 = []),
                                        include(arity_meta(N), Metas0, Metas),
                                        Metas \== [],
                                        findall(ATs, fn_decl_arity(F, N, ATs, _), [ATs1]),
                                        arrow_det_positions(ATs1, Positions),
                                        Positions \== [],
                                        maplist(assume_det_meta(ATs1, Positions), Metas, Upgraded),
                                        catch(b_getval('$det_assume_stack', St), _, St = []),
                                        b_setval('$det_assume_stack', [F|St]),
                                        det_enforced_flag(F, N, Enf),
                                        with_det_enforced(Enf, clause_set_determinism(Upgraded, Det0)),
                                        b_setval('$det_assume_stack', St),
                                        Det = Det0,
                                        assertz(det_assume_cache(F, N, Det)).

%Copy the clause meta (attributes copy with the term) and, at each arrow
%position, attach the det form of the declared arrow to the COPIED head var -
%never the stored one:
assume_det_meta(ATs1, Positions, Meta, Meta2) :- copy_term(Meta, Meta2),
                                                 Meta2 = fun_meta(Args, _),
                                                 maplist(bind_meta_param, Args, ATs1),
                                                 assume_det_positions(Positions, ATs1, Args).

assume_det_positions([], _, _).
assume_det_positions([pos(Idx, _, _)|Ps], ATs1, Args) :- nth0(Idx, ATs1, T), T = [_|Rest],
                                                        copy_term(Rest, Rest2),
                                                        det_arrow_head(det, DetHead),
                                                        DetArrow = [DetHead|Rest2],
                                                        ( nth0(Idx, Args, HeadArg) -> assume_det_param(HeadArg, DetArrow) ; true ),
                                                        assume_det_positions(Ps, ATs1, Args).

%Only a var head param (carrying no attr, or an existing arrow attr) is
%upgraded; any other shape is left as-is so the analysis stays conservative
%(and therefore sound) for that clause:
assume_det_param(V, DetArrow) :- ( var(V),
                                   ( get_attr(V, tknown, [K]) -> ( nonvar(K), is_arrow_type(K) ) ; true )
                                 -> put_attr(V, tknown, [DetArrow])
                                 ; true ).

%Underapplication builds a closure instead of calling (reduce case 1):
%constructing the partial is deterministic - the closure's own determinism
%is judged at its call site through its arrow type - so only the bound
%arguments need to be deterministic here:
underapplied_closure(Fun, N) :- CallArity is N + 1,
                                \+ arity(Fun, CallArity),
                                arity(Fun, Known), Known > CallArity, !.

%%% Combining determinism verdicts. The lattice is
%%% ok < may_fail(_) < nondeterministic(_) / unknown(_): a subexpression that
%%% may fail leaves the whole expression semidet, while a branching or opaque
%%% one settles it - so may_fail keeps scanning for something worse, and the
%%% top of the lattice short-circuits (which preserves the historical
%%% "first non-ok verdict wins" reason reporting):
%once/1 (which is what (once E) compiles to) caps the solution count at one; it
%never manufactures one. So it does erase nondeterminism - and opacity too: an
%expression nothing can analyse still has AT MOST one solution once wrapped -
%but it does not erase failure, because (once E) fails exactly when E does.
%The old reading, that (once E) is unconditionally ok, threw the callee's
%may_fail away and let a -[semidet]-> call satisfy a -[det]-> promise.
%Note this is a REFINEMENT as well as a tightening: once(nondeterministic) and
%once(unknown) used to be discarded, and are now the strictly more precise
%may_fail (zero or one), which -[semidet]-> accepts.
once_determinism(Expr, Result) :- deterministic_expr(Expr, R),
                                  ( R == ok -> Result = ok
                                  ; R = may_fail(_) -> Result = R
                                  ; R = nondeterministic(Why) -> Result = may_fail(once(Why))
                                  ; R = unknown(Why) -> Result = may_fail(once(Why))
                                  ; Result = may_fail(once(R)) ).

det_result_rank(ok, 0).
det_result_rank(may_fail(_), 1).
det_result_rank(nondeterministic(_), 2).
det_result_rank(unknown(_), 2).

det_result_final(R) :- det_result_rank(R, 2).

combine_det_results(A, B, R) :- det_result_rank(A, RA), det_result_rank(B, RB),
                                ( RB > RA -> R = B ; R = A ).

combine_determinism_list([], ok).
combine_determinism_list([Expr|Exprs], Result) :- deterministic_expr(Expr, First),
                                                  ( det_result_final(First) -> Result = First
                                                  ; combine_determinism_list(Exprs, Rest),
                                                    combine_det_results(First, Rest, Result) ).

%(let* ((P1 V1) (P2 V2) ...) Body) IS nested (let P1 V1 (let P2 V2 ...)),
%so each binding is judged by let_determinism/4 itself - the destructured
%field types, the collapse properness guarantee and the nonemptiness
%narrowing all apply per binding with no second copy of the logic. The old
%pattern_then_exprs walk analyzed each pair in isolation, which is exactly
%how the let refinements failed to reach let*:
binds_and_body_determinism([], Body, Result) :- deterministic_expr(Body, Result).
binds_and_body_determinism([[Pat, Val]|Rest], Body, Result) :-
    ( Rest == [] -> In = Body ; In = ['let*', Rest, Body] ),
    let_determinism(Pat, Val, In, Result).

case_expr_determinism(KeyExpr, PairsExpr, Result) :- deterministic_expr(KeyExpr, KeyResult),
                                                     ( det_result_final(KeyResult) -> Result = KeyResult
                                                     ; case_pairs_determinism(PairsExpr, R2),
                                                       case_coverage_determinism(KeyExpr, PairsExpr, R3),
                                                       combine_det_results(KeyResult, R2, R12),
                                                       combine_det_results(R12, R3, Result) ).

%%% Does the case cover its scrutinee?
%%%
%%% translate_case/6 compiles the branches to a nested if-then-else with NO
%%% final else, so a value that matches no pattern makes the whole case FAIL.
%%% That failure path belongs to the construct, not to any branch, so
%%% case_pairs_determinism/2 above cannot see it.
%%%
%%% The verdict is ASYMMETRIC in exactly the way det_exhaustiveness_prepass/1
%%% is, and for the same reason: PeTTa's nominal types are OPEN, so "cannot
%%% tell" is the common case and treating it as failure would reject most
%%% legitimate code. Only a PROVABLY uncovered value yields may_fail - a
%%% scrutinee whose type is unknown, unenumerable or extensible stays silent.
%%% The proof itself is unmatched_case/5, the same relation the clause-head
%%% exhaustiveness check uses, applied to the branch patterns as a one-column
%%% head set.
case_coverage_determinism(KeyExpr, PairsExpr, Result) :-
    ( case_scrutinee_type(KeyExpr, T0), copy_term(T0, T),
      case_value_patterns(PairsExpr, Heads), Heads \== [],
      catch(unmatched_case([], Heads, 0, T, Missing0), _, fail),
      copy_term(Missing0, Missing)
      -> Result = may_fail(nonexhaustive_case(Missing))
       ; Result = ok ).

%The scrutinee's type, when the checker already knows it: a parameter (or any
%other variable) carrying a single known type, or a call to a function with a
%unique declaration at that arity. Anything else has no type here and the
%coverage question is simply not asked.
case_scrutinee_type(K, T) :- var(K), !, known_singleton(K, T0), nonvar(T0), T = T0.
case_scrutinee_type(K, T) :- nonvar(K), is_list(K), K = [F|Args], atom(F), fun(F),
                             length(Args, N),
                             findall(OT, fn_decl_arity(F, N, _, OT), [T0]),
                             nonvar(T0), T = T0.

%The branch patterns, as single-argument "clause heads" for unmatched_case/5.
%The (Empty ...) branch is dropped: it is not a value pattern at all but the
%fallback translate_expr/3 wires to "the KEY produced no solution", so it
%covers nothing the other branches leave open.
case_value_patterns(Pairs, Heads) :- is_list(Pairs),
                                     findall([P], ( member(Pair, Pairs), nonvar(Pair),
                                                    Pair = [P, _], P \== 'Empty' ),
                                             Heads).

case_pairs_determinism([], ok).
case_pairs_determinism([[CaseExpr, BranchExpr]|Rest], Result) :-
    pattern_then_exprs(CaseExpr, [BranchExpr], PairResult),
    ( det_result_final(PairResult) -> Result = PairResult
    ; case_pairs_determinism(Rest, R2),
      combine_det_results(PairResult, R2, Result) ).

%Patterns are matched, not executed: a variable head is structure, and only
%fun-headed subterms are embedded calls that the pattern evaluates:
pattern_then_exprs(Pat, Exprs, Result) :- deterministic_pattern(Pat, R0),
                                          ( det_result_final(R0) -> Result = R0
                                          ; combine_determinism_list(Exprs, R2),
                                            combine_det_results(R0, R2, Result) ).

%(let Pat Val In) / (chain Pat Val In). Same worst-of composition as
%pattern_then_exprs, but a DESTRUCTURING pattern's field variables are bound to
%FIELDS of Val's result, and when Val's declared output type fixes a field to a
%concrete NON-arrow type that field can never be a function symbol (a well-typed
%(Number Number) tuple holds numbers). So a body expression HEADED by such a
%field - the (let ($l $r) (add-pair ..) ($l $r)) reconstruction - is data, not
%a dynamic dispatch: reduce/2 leaves a non-function (or unbound) head
%unevaluated, exactly one solution. We give the body analysis that knowledge by
%binding the field types onto COPIES of the pattern/body variables (never the
%shared source term), so the var-head clause reads them as data construction.
let_determinism(Pat, Val, In, Result) :-
    deterministic_pattern(Pat, R0),
    ( det_result_final(R0) -> Result = R0
    ; deterministic_expr(Val, RVal),
      ( det_result_final(RVal) -> Result = RVal
      ; copy_term(Pat-In, PatC-InC),
        ignore(bind_destructured_field_types(PatC, Val)),
        %A PLAIN-var pattern bound to a value that is GUARANTEED a proper
        %list - a collapse form (findall/3 output) or a call to a
        %proper_list_output-certified function - carries that knowledge into
        %the body analysis, so the (== $v ()) nonemptiness narrowing can fire
        %on a let-introduced variable exactly as it does on a declared list
        %parameter. The guarantee is load-bearing: the narrowing's coverage
        %leg assumes the runtime value IS a list (a cons head cannot match a
        %Number, and a miss is a failure under det), and collapse is what
        %makes that unconditional. A declared (List _) output does NOT
        %qualify - typed is not bound, so no certificate, no knowledge:
        ( var(PatC), val_guaranteed_proper_list(Val)
          -> add_known_type(PatC, ['List', '%Undefined%']) ; true ),
        %fields whose type stayed unknown (arrow, wildcard, no declared tuple
        %type at all) can arrive bound to anything, functions included - mark
        %the copies so they read as parameters, not as fresh locals:
        term_variables(PatC, FVs),
        maplist(mark_field_unless_typed, FVs),
        deterministic_expr(InC, RIn),
        combine_det_results(RVal, RIn, RVI),
        combine_det_results(R0, RVI, Result) ) ).

%Bind each destructuring field variable to its concrete non-arrow field type,
%read off Val's declared tuple output type. Non-arrow, non-wildcard only: an
%arrow or Atom field could legitimately carry a function, so it stays unknown.
bind_destructured_field_types(Pat, Val) :-
    nonvar(Pat), Pat = [At, Whole, Inner], At == '@', !,
    call_output_type(Val, OT),
    bind_det_pattern_type(Whole, OT),
    bind_det_pattern_type(Inner, OT).
bind_destructured_field_types(Pat, Val) :-
    is_list(Pat), Pat = [_|_],
    call_output_type(Val, OT),
    is_list(OT), same_length(Pat, OT),
    bind_pat_field_types(Pat, OT).

bind_det_pattern_type(P, T) :- ( var(P), nonvar(T), \+ is_arrow_type(T), \+ wildcard_type_t(T)
                                 -> add_known_type(P, T)
                                ; nonvar(P), P = [At, Whole, Inner], At == '@'
                                  -> bind_det_pattern_type(Whole, T),
                                     bind_det_pattern_type(Inner, T)
                                ; is_list(P), is_list(T), same_length(P, T),
                                  \+ is_arrow_type(T)
                                  -> bind_pat_field_types(P, T)
                                ; true ).

bind_pat_field_types([], []).
bind_pat_field_types([P|Ps], [T|Ts]) :- ( var(P), nonvar(T), \+ is_arrow_type(T), \+ wildcard_type_t(T)
                                          -> add_known_type(P, T) ; true ),
                                        bind_pat_field_types(Ps, Ts).

mark_field_unless_typed(V) :- ( get_attr(V, tknown, _) -> true ; note_unknown_candidate(V) ).

call_output_type([F|Args], OT) :- atom(F), length(Args, N), fn_decl_arity(F, N, _, OT), nonvar(OT).

deterministic_pattern(P, ok) :- ( var(P) ; atomic(P) ; P = partial(_, _) ), !.
deterministic_pattern([H|T], Result) :- atom(H), fun(H), !, deterministic_expr([H|T], Result).
deterministic_pattern(P, Result) :- combine_pattern_list(P, Result).

combine_pattern_list([], ok).
combine_pattern_list([E|Es], Result) :- deterministic_pattern(E, R1),
                                        ( det_result_final(R1) -> Result = R1
                                        ; combine_pattern_list(Es, R2),
                                          combine_det_results(R1, R2, Result) ).

%%% Exhaustiveness of -[det]-> functions (--strict-det only) %%%
%%%
%%% -[det]-> promises EXACTLY one result, but a clause set that cannot match
%%% some input of its declared argument types delivers zero. The check is
%%% deliberately ASYMMETRIC: provably incomplete is an error, cannot tell is
%%% accepted in silence. PeTTa's nominal types are OPEN - a constructor may be
%%% declared in a later file - so "cannot tell" is the common case, and
%%% GHC-style "warn unless proven exhaustive" would reject legitimate code
%%% with no way out. The way out of a REAL incompleteness is -[semidet]->,
%%% which commits (and therefore costs) exactly like -[det]->.
%%%
%%% Only an EXPLICIT -[det]-> is checked (explicit_det_decl/2). Under
%%% --strict-det a plain -> also reads as det, but that is a mode-wide default
%%% rather than a per-function promise, and holding every -> to totality would
%%% reject most partial helpers in the standard library.
%%%
%%% Because the promise is per-function and written down, it is checked in
%%% EVERY mode - like the overlap and body-determinism checks an explicit
%%% -[det]-> already gets flaglessly. --strict-det forces you to make the
%%% determinism claim; it is not what makes a claim you already made mean
%%% something. If you did not mean det, write -> and nothing is checked.
%%%
%%% This runs as a per-file PREPASS over the parsed forms (filereader.pl), for
%%% the same reason type declarations are pre-cached there: exhaustiveness is a
%%% property of the WHOLE clause set, and clauses arrive one form at a time -
%%% checking after each one would reject (= (not true) false) before
%%% (= (not false) true) has been read. Clauses already compiled by earlier
%%% files count too (stored_clause_head/3), but a function whose clauses are
%%% split across files is still judged on what the current file can see.
det_exhaustiveness_prepass(ParsedForms) :-
    findall(F/N, ( parsed_clause_head(ParsedForms, _, _, F, Args), length(Args, N) ), Keys0),
    sort(Keys0, Keys),
    %value declarations are order-sensitive knowledge atoms, so unlike
    %arrow declarations they are NOT pre-cached - the file's own nullary
    %constructors are read straight from its forms instead:
    findall(C-T, parsed_value_decl(ParsedForms, C, T), Consts),
    forall(member(F/N, Keys), check_det_exhaustive_group(ParsedForms, Consts, F, N)).

parsed_clause_head(ParsedForms, Line, Str, F, Args) :-
    member(parsed(function, Str, Line, Form), ParsedForms),
    nonvar(Form), Form = [Eq, Head, _], Eq == (=),
    nonvar(Head), Head = [F|Args], atom(F), Args \== [].

parsed_value_decl(ParsedForms, C, T) :- member(parsed(expression, _, _, Form), ParsedForms),
                                        nonvar(Form), Form = [Colon, C, T], Colon == (:),
                                        atom(C), atom(T), \+ fun(C).

stored_clause_head(F, N, Args) :- catch(nb_getval(F, Metas), _, fail),
                                  member(fun_meta(Args, _), Metas), length(Args, N).

check_det_exhaustive_group(ParsedForms, Consts, F, N) :-
    ( explicit_det_decl(F, N)
      -> findall(Args, ( parsed_clause_head(ParsedForms, _, _, F, Args), length(Args, N)
                       ; stored_clause_head(F, N, Args) ), Heads),
         once(( parsed_clause_head(ParsedForms, Line, Str, F, A0), length(A0, N) )),
         %the verdict is a snapshot of the constructor sets it consulted, so
         %it is kept along with WHICH sets those were - a constructor declared
         %later re-runs exactly the verdicts its type takes part in:
         with_ctor_snapshot_types(
             with_form_location(Line, Str, check_det_exhaustive(Consts, F, N, Heads)), Types),
         current_metta_file(File),
         retractall(det_exhaustive_verdict(F, N, _, _, _, _, _, _)),
         assertz(det_exhaustive_verdict(F, N, Heads, Consts, Types, File, Line, Str))
       ; true ).

%The declaration must be unique at this arity: several declarations are typed
%overloads, and the clauses then belong to no single argument-type vector.
check_det_exhaustive(Consts, F, N, Heads) :-
    ( findall(ATs, fn_decl_arity(F, N, ATs, _), [ATs1]),
      nth0(Idx, ATs1, T),
      unmatched_case(Consts, Heads, Idx, T, Missing)
      -> Pos is Idx + 1,
         throw(error(det_nonexhaustive(F, Pos, Missing), determinism))
       ; true ).

%One argument position proves incompleteness when EVERY clause pins it to a
%recognizable shape - no variable, no wildcard, no computed subterm - and some
%value of its declared type has no matching shape. A single variable in the
%column, an unenumerable type, or a pattern the key relation does not
%recognize makes the position silent; guards and arithmetic conditions live in
%the body and are never consulted (they can only make a function match LESS,
%so ignoring them keeps the verdict sound).
unmatched_case(Consts, Heads, Idx, T, Missing) :-
    nonvar(T), \+ wildcard_type_t(T),
    findall(P, ( member(H, Heads), nth0(Idx, H, P) ), Col),
    Col \== [],
    maplist(pattern_key, Col, Keys),
    ( uncovered_infinite_domain(T, Keys) -> Missing = other(T)
    ; domain_keys(T, Consts, DKeys), member(Missing, DKeys), \+ memberchk(Missing, Keys) ).

%The value shape a head pattern matches, as key(Name, Arity). A variable, a
%fun-headed (computed) subterm, an empty expression or anything else has no
%key - and a column containing one is no evidence at all:
pattern_key(P, key(P, 0)) :- atomic(P), P \== [], \+ ( atom(P), fun(P) ), !.
pattern_key(P, key(C, K)) :- is_list(P), P = [C|As], atom(C), \+ fun(C),
                             length(As, K), K > 0.

%Number and String have infinitely many values, so a column of literals of
%that domain can never cover them:
uncovered_infinite_domain('Number', Keys) :- forall(member(key(V, A), Keys), ( A =:= 0, number(V) )).
uncovered_infinite_domain('String', Keys) :- forall(member(key(V, A), Keys), ( A =:= 0, string(V) )).

%The complete set of value shapes of a type, when it can be enumerated at all.
%Bool is closed by construction; a nominal type's set is its equation-less
%declared constructors (member_ctor/3 - a declared symbol WITH equations is
%rewritten at the call site and never survives as a value) plus its
%equation-less declared constants.
%The set is read as it stands right now, so like union_member_excluded/3 this
%is a SNAPSHOT: a constructor for T declared in a later file changes it. Both
%users of the snapshot are recorded and re-read when that happens - a case
%coverage verdict through note_ctor_snapshot/1 (it runs inside a clause
%translation, so the clause can be recompiled), the clause-head exhaustiveness
%verdict through det_exhaustive_verdict/7 (it does not, so it is re-run).
domain_keys('Bool', _, [key(true, 0), key(false, 0)]) :- !.
domain_keys(T, Consts, Keys) :- atom(T), declared_newtype(T, R), !, domain_keys(R, Consts, Keys).
domain_keys(T, Consts, Keys) :- atom(T), \+ wildcard_type(T), \+ primitive_type(T),
                                note_ctor_snapshot(T),
                                findall(key(C, K), nominal_ctor(T, Consts, C, K), Keys0),
                                sort(Keys0, Keys), Keys \== [].

nominal_ctor(T, _, C, K) :- member_ctor(T, K, C).
nominal_ctor(T, _, C, 0) :- declared_value_type(C, T2), atom(C), T2 == T, \+ fun(C).
nominal_ctor(T, Consts, C, 0) :- member(C-T, Consts).

%Rendered by main.pl's error message for det_nonexhaustive/3:
missing_case_text(other(T), Txt) :- !, format(atom(Txt), "a ~w outside the matched literals", [T]).
missing_case_text(key(C, 0), Txt) :- !, format(atom(Txt), "~w", [C]).
missing_case_text(key(C, K), Txt) :- length(As, K), maplist(=('_'), As),
                                     atomic_list_concat([C|As], ' ', Inner),
                                     format(atom(Txt), "(~w)", [Inner]).

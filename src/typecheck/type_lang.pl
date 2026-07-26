%%% The single compatibility relation. Binds type variables (polymorphism);
%%% wrap in type_compat_soft/2 for a side-effect-free check.
wildcard_type('%Undefined%').
wildcard_type('Atom').
wildcard_type('Expression').
wildcard_type_t(T) :- atom(T), wildcard_type(T).

type_unify(A, B) :- ( var(A) ; var(B) ), !, A = B.
%A wildcard is NOT "every type at once" - it is "nothing is stated here". The
%difference is invisible for structural types (a wildcard on either side
%simply discharges the obligation) but load-bearing against a newtype brand,
%so the brand rules run BEFORE the wildcard shortcut and the shortcut never
%sees a brand on either side. See brand_unify/2:
type_unify(A, B) :- ( brand_name(A) ; brand_name(B) ), !, brand_unify(A, B).
type_unify(A, B) :- ( wildcard_type_t(A) ; wildcard_type_t(B) ), !.
%Union types (| T1 T2 ...): a union value must fit every context member-wise;
%a value fits a required union if it fits some member:
type_unify(A, B) :- is_union(A), !, A = ['|'|As],
                    \+ ( member(MA, As), \+ type_compat_soft(MA, B) ).
type_unify(A, B) :- is_union(B), !, B = ['|'|Ms],
                    member(M, Ms), type_unify(A, M), !.
type_unify(A, B) :- atom(A), !, A == B.
%Arrows: a det closure fits anywhere, a nondet closure only fits a nondet
%requirement once --strict-det makes plain -> a determinism commitment:
type_unify(A, B) :- is_arrow_type(A), is_arrow_type(B), !,
                    A = [HA|As], B = [HB|Bs],
                    det_arrow_fits(HA, HB),
                    same_length(As, Bs), maplist(type_unify, As, Bs).
type_unify(A, B) :- is_list(A), !, is_list(B), same_length(A, B), maplist(type_unify, A, B).
type_unify(A, B) :- A == B.

brand_name(T) :- atom(T), declared_newtype(T, _).

%%% (Newtype R) is NOMINAL, and the rule is one-directional and non-vacuous:
%%%
%%%   1. A brand fits itself, and no other brand.
%%%   2. A brand fits a WILDCARD requirement (%Undefined%/Atom/Expression).
%%%      That is what erasure means: at runtime the value simply is its
%%%      representation, and a wildcard requirement asks for nothing.
%%%   3. A brand fits a concrete requirement T exactly when its representation
%%%      does - PROVIDED the representation is not itself a wildcard. A
%%%      wildcard representation says "the payload shape is unconstrained",
%%%      and reading that as "the payload fits every type" would erase the
%%%      brand into a universal type: (: Proof (Newtype Expression)) made Proof
%%%      and Number mutually compatible, so a Proof could be handed to
%%%      (-> Number Number) with the obligation discharged statically.
%%%      Unconstrained is not the same as universal.
%%%   4. NOTHING implicitly fits a brand - not a wildcard either. The whole
%%%      point of a brand is that a value acquires it in exactly one way, by
%%%      being written (brand T V); an unknown or unconstrained value is not
%%%      evidence that the brand was ever applied.
%%%
%%% (3) and (4) are what closed the hole; (1) and (2) are the previous
%%% behaviour restated. Note the asymmetry is deliberate and matches the
%%% relation's argument convention: type_unify(Actual, Required).
brand_unify(A, B) :- brand_name(A), brand_name(B), !, A == B.
brand_unify(A, B) :- brand_name(B), !,
                     %a union VALUE fits a brand only if every member does,
                     %mirroring the is_union(A) rule below; nothing else does:
                     is_union(A), A = ['|'|As],
                     \+ ( member(MA, As), \+ type_compat_soft(MA, B) ).
brand_unify(_, B) :- wildcard_type_t(B), !.
brand_unify(A, B) :- is_union(B), !, B = ['|'|Ms], member(M, Ms), type_unify(A, M), !.
brand_unify(A, B) :- declared_newtype(A, RA), \+ wildcard_type_t(RA), type_unify(RA, B).

%A closure fits a required arrow when it can produce no MORE results than the
%requirement allows (det < semidet < nondet). An explicit -[det]->/-[semidet]->
%requirement is a commitment in every mode, so a nondet closure never fits it;
%a plain -> claims nothing outside --strict-det, where it becomes det:
det_arrow_fits(HA, HB) :- arrow_atom_det(HA, LA), arrow_atom_det(HB, LB),
                          det_level_fits(LA, LB).

det_level_fits(_, nondet) :- !.
det_level_fits(LA, plain) :- !, ( LA == nondet -> \+ strict_det(true) ; true ).
det_level_fits(LA, LB) :- ( LA == plain -> strict_det(true)
                          ; LA == det -> true
                          ; LA == semidet -> LB == semidet ).

is_union(T) :- nonvar(T), T = [P|_], P == '|'.

type_compat_soft(A, B) :- \+ \+ type_unify(A, B).

is_arrow_type(T) :- nonvar(T), T = [A|_], arrow_atom(A).

list_type(T, ET) :- nonvar(T), T = [L, ET], L == 'List'.

%%% Attribute hooks (permissive merging; errors are raised by explicit checks):
tknown:attr_unify_hook(Cs, Other) :-
    ( var(Other) -> ( get_attr(Other, tknown, C2) -> variant_union(Cs, C2, U),
                                                     put_attr(Other, tknown, U)
                                                   ; put_attr(Other, tknown, Cs) )
                  ; true ).

mreq:attr_unify_hook(Rs, Other) :-
    ( var(Other) -> ( get_attr(Other, mreq, R2) -> forall(member(A, Rs),
                                                          forall(member(B, R2), type_compat_soft(A, B))),
                                                   append(Rs, R2, U),
                                                   put_attr(Other, mreq, U)
                                                 ; put_attr(Other, mreq, Rs) )
                  ; forall(member(R, Rs), \+ value_definitely_mismatch(Other, R)) ).

variant_member(X, [Y|_]) :- X =@= Y, !.
variant_member(X, [_|T]) :- variant_member(X, T).

variant_union([], Ys, Ys).
variant_union([X|Xs], Ys, U) :- ( variant_member(X, Ys) -> variant_union(Xs, Ys, U)
                                                         ; variant_union(Xs, [X|Ys], U) ).

%%% Translation-time known types of variables:
add_known_type(V, T) :- nonvar(T), unknown_candidate(T), !, note_unknown_candidate(V).
add_known_type(V, T) :- ( get_attr(V, tknown, Cs) -> ( Cs = [K], var(K) -> K = T
                                                      ; variant_member(T, Cs) -> true
                                                      ; put_attr(V, tknown, [T|Cs]) )
                                                   ; put_attr(V, tknown, [T]) ).

known_candidates(V, Cs) :- get_attr(V, tknown, Cs).
%A candidate set containing the unknown marker is NOT a singleton type: some
%flow into this variable carried a value of undetermined type, so nothing may
%be discharged from the types the other flows happened to contribute.
known_singleton(V, K) :- get_attr(V, tknown, [K]), \+ unknown_candidate(K).

%%% The unknown-branch marker.
%
% A construct that merges several branches into one result (if, case,
% let/chain, sealed, superpose, hyperpose) records each branch's type as a
% candidate of the merged variable. A branch whose type the checker cannot
% determine used to record NOTHING, which made "some recorded candidate fits"
% - an existential test - look like a proof about the whole disjunction: one
% typed branch discharged the obligation for all of them, and an untyped
% branch could then deliver a value of any type where a concrete one was
% certified. The marker makes the untyped branch visible, so the discharge
% tests are effectively universal again: a merged variable carrying it is
% never a known singleton, and an output certification over it falls back to a
% runtime guard (a hard rejection under --strict).
unknown_marker('$unknown_branch_type').
%Never unifies: a candidate list legitimately holds unbound declaration-instance
%type variables, and testing them must not bind one to the marker.
unknown_candidate(C) :- unknown_marker(M), C == M.
%A wrapped GROUND data literal a merge could not assign a single type to - (a b)
%fed to a (| Number (List Atom)) output. value_single_type/2 fails on it (its
%bare-atom elements carry no declared type), so an ordinary merge would record
%the plain unknown marker. The literal is instead recorded wrapped: it counts as
%unknown_candidate/1 EVERYWHERE (a merged variable carrying it is never a known
%singleton, arg passing and parametric checks stay conservative), and is only
%read specially at the output certification (output_candidate_fits/2), where the
%concrete target type is in hand and check_value/3 can certify the actual value.
unknown_candidate(C) :- certifiable_literal_candidate(C, _).
%nonvar guard is load-bearing: unknown_candidate/1 and the output-check discharge
%are called with candidates that may be unbound type variables (param promises),
%and this must TEST such a candidate, never bind it to the wrapper.
certifiable_literal_candidate(C, V) :- nonvar(C), C = '$certifiable_literal'(V).

%%% Indefinite evidence: a PROMISED type variable.
%
% An obligation may only be discharged by EVIDENCE, and one particular kind of
% unbound type variable is not evidence: one this clause's own declaration
% promised to its callers (see param_promise_var/1). Its value is whatever the
% CALLER chose, so answering a concrete requirement by unifying with it is the
% same confusion the unknown marker was introduced to fix for branch merges -
% "I don't know" read as "compatible with everything":
%
%     (: g (-> (-> Number $b) Number))
%     (= (g $f) ($f 1))                    % result type is $b, unbound
%
% $b is whatever the caller's closure returns, yet type_compat_soft($b,
% 'Number') succeeded and certified the clause's output as Number with no
% guard, under --strict. (g h) with (: h (-> Number String)) then handed a
% string to arithmetic.
%
% NOT every unbound candidate is indefinite. An output type variable occurring
% in none of its function's argument types is universally quantified, so by
% parametricity only a bottom implementation can produce it - (: empty (-> $a))
% is the one in the standard library - and a value that is never produced fits
% every requirement. set_call_out_type/3 already makes exactly that
% distinction; this is its counterpart on the reading side.
%
% The var is deliberately NOT replaced by the marker: its identity is
% load-bearing (it aliases the declaration instance shared with the context -
% map-flat's element type - and known_singleton/2 with a var K is consulted on
% purpose by ascribe_type/3). Nor does type_compat_soft/2 change: it is also
% the definite-CONFLICT test, where refusing a var would turn "unknown" into
% "wrong". The distinction belongs at the discharge test, so it lives here.
indefinite_candidate(C) :- var(C), !, param_promise_var(C).
indefinite_candidate(C) :- unknown_candidate(C).

note_unknown_candidate(V) :- ( var(V) -> unknown_marker(M),
                                         ( get_attr(V, tknown, Cs)
                                           -> ( variant_member(M, Cs) -> true
                                              ; put_attr(V, tknown, [M|Cs]) )
                                            ; put_attr(V, tknown, [M]) )
                                       ; true ).

candidates_have_unknown(Cs) :- member(C, Cs), unknown_candidate(C), !.

%Drop the marker; fails if it was there at all, so callers that can make no
%claim about a partly unknown value simply make none:
known_candidates_certain(V, Cs) :- known_candidates(V, Cs), \+ candidates_have_unknown(Cs).

%Propagate Val's statically known type(s) into Out (branch and binding flows).
%A value whose type cannot be determined propagates the unknown marker - that
%is knowledge too, and the only kind that keeps a merge honest:
note_candidates(Out, Val) :- ( var(Out)
                               -> ( nonvar(Val) -> ( value_single_type(Val, VT)
                                                     -> add_known_type(Out, VT)
                                                      ; ground(Val), is_list(Val)
                                                        -> note_certifiable_literal(Out, Val)
                                                      ; note_unknown_candidate(Out) )
                                  ; known_candidates(Val, Cs) -> add_known_types(Out, Cs)
                                  ; note_unknown_candidate(Out) )
                                ; true ).

%Record a ground literal a merge could not type as a certifiable-literal
%candidate (see unknown_candidate/1). Mirrors note_unknown_candidate/1 but keeps
%the value so the output certification can check it against the concrete target.
note_certifiable_literal(V, Val) :- ( var(V)
                                      -> M = '$certifiable_literal'(Val),
                                         ( get_attr(V, tknown, Cs)
                                           -> ( variant_member(M, Cs) -> true
                                              ; put_attr(V, tknown, [M|Cs]) )
                                            ; put_attr(V, tknown, [M]) )
                                       ; true ).

%Explicit type ascription (the Type Expr): the author states the type of a
%dynamically typed value. The type becomes knowledge for the checker, and a
%runtime check is emitted even under --strict: strict mode forbids *implicit*
%residual checks, while an ascription is an explicit, visible boundary.
%An ascription that contradicts static knowledge is a compile-time error.
ascribe_type(V, T, Gs) :- ( var(T) -> Gs = []
                          ; wildcard_type_t(T) -> Gs = []
                          ; var(V) ->
                              %an unknown branch fed this value, but the
                              %ascription's own runtime guard establishes T
                              %where the merge could not: take the ascription
                              %as the answer and drop the marker
                              ( known_candidates(V, Cs0), candidates_have_unknown(Cs0), ground(T)
                                -> put_attr(V, tknown, [T]), ascription_guard(V, T, Gs)
                              ; known_singleton(V, K), var(K)
                                %the value's only known type is a bare declaration-instance
                                %variable: narrow it locally WITHOUT binding that variable, so a
                                %parametric parameter stays universally quantified (its callers
                                %remain unchecked) while this explicit (the ...) boundary still
                                %emits the runtime guard - the ascription is honest, not a
                                %concrete-type requirement leaking into the declaration:
                                -> put_attr(V, tknown, [T]), ascription_guard(V, T, Gs)
                              ; known_singleton(V, K)
                                -> ( type_unify(K, T) -> Gs = []
                                   ; \+ \+ type_unify(T, K)       %the ascribed type fits the known type
                                     -> put_attr(V, tknown, [T]), %(e.g. a union member): narrow to it, checked
                                        ascription_guard(V, T, Gs)
                                   ; throw(error(type_conflict(existing(K), required(T)), typecheck)) )
                                 ; add_known_type(V, T),
                                   ascription_guard(V, T, Gs) )
                          ; check_value(V, T, St),
                            ( St == ok -> Gs = []
                            ; St == mismatch -> throw(error(literal_type_mismatch(V, T), typecheck))
                            ; ascription_guard(V, T, Gs) ) ).

ascription_guard(V, T, Gs) :- ( ground(T) -> warn_residual_check('(the ...)', T),
                                             guard_goal(V, T, G), Gs = [G]
                                           ; Gs = [] ).

%(brand T Expr): erased trust in a semantic role. No runtime goal exists - a
%role has no runtime witness by construction - but a value already carrying
%a DIFFERENT brand is rejected, and the value must be statically admissible
%for the newtype's representation:
brand_type(V, T) :-
    ( \+ ( atom(T), declared_newtype(T, _) )
      -> throw(error(unknown_newtype(T), typecheck))
    ; var(V) -> ( known_singleton(V, K)
                  -> ( type_unify(K, T) -> true
                     ; atom(K), declared_newtype(K, _)
                       -> throw(error(type_conflict(existing(K), required(T)), typecheck))
                     ; declared_newtype(T, R), \+ \+ type_unify(K, R)
                       -> put_attr(V, tknown, [T])
                     ; throw(error(type_conflict(existing(K), required(T)), typecheck)) )
                   ; add_known_type(V, T) )
    ; check_value(V, T, St),
      ( St == mismatch -> throw(error(literal_type_mismatch(V, T), typecheck)) ; true ) ).

%Newtype-transparent test for the Expression argument convention (arguments
%whose declared type is Expression, or a brand of it, stay unevaluated data):
expression_typed(Ty) :- nonvar(Ty), ( Ty == 'Expression' -> true
                                    ; atom(Ty), declared_newtype(Ty, R), R == 'Expression' ).

%Derive match-pattern variable types from declared relation schemas: atoms
%matched by (F ...) conform to F's declared argument types, and a pattern
%(: $x T) binds $x : T directly. Conjunctive patterns type each conjunct.
type_match_pattern(P) :- ( is_list(P) -> type_match_pattern_list(P) ; true ).

type_match_pattern_list([C, V, Ty]) :- C == (:), var(V), nonvar(Ty), !,
                                       normalize_type(Ty, TN),
                                       ( \+ wildcard_type_t(TN) -> add_known_type(V, TN) ; true ).
type_match_pattern_list([C|Ps]) :- C == ',', !, maplist(type_match_pattern, Ps).
type_match_pattern_list([F|Args]) :- atom(F), length(Args, N),
                                     findall(ATs, fn_decl_arity(F, N, ATs, _), [ATs1]), !,
                                     maplist(bind_pattern_arg, Args, ATs1).
type_match_pattern_list(_).

bind_pattern_arg(V, T) :- var(V), !, ( nonvar(T), \+ wildcard_type_t(T) -> add_known_type(V, T) ; true ).
bind_pattern_arg(A, _) :- type_match_pattern(A).

%Type the element variable of a higher-order construct from its list argument:
note_list_elem_type(XVar, L) :-
    ( var(XVar), list_elem_type(L, ET) -> add_known_type(XVar, ET) ; true ).

list_elem_type(L, ET) :- var(L), !, known_singleton(L, ['List', ET0]), ground(ET0), ET = ET0.
%A variable element type is fine for literal lists: it is a declaration
%instance var (e.g. rcons appending an $a onto a (List $a)), compared by
%identity so distinct unknowns do not conflate:
list_elem_type(L, ET) :- is_list(L), L = [E|Es],
                         value_single_type(E, ET),
                         forall(member(E2, Es), ( value_single_type(E2, T2), T2 == ET )).

add_known_types(V, Cs) :- maplist(add_known_type(V), Cs).

set_out_type(Out, OT) :- ( var(Out), nonvar(OT), \+ wildcard_type_t(OT) -> add_known_type(Out, OT)
                                                                          ; true ).

%When F/N has exactly one declared output type, the call result is that type:
set_unique_decl_out(F, N, Out) :- ( atom(F), findall(OT, fn_decl_arity(F, N, _, OT), [OT1])
                                    -> set_out_type(Out, OT1) ; true ).

%Manual call/reduce dispatch bypasses typed translation, not typing: when the
%target has exactly one declaration at this arity, its input checks apply:
manual_dispatch_arg_checks(F, N, AVs, Gs) :- ( atom(F), findall(ATs, fn_decl_arity(F, N, ATs, _), [ATs1])
                                               -> apply_call_args(declared, F, AVs, ATs1, Gs)
                                                ; Gs = [] ).

%Call-site output typing. An output type variable that occurs in no argument
%type is universally quantified - by parametricity only a bottom function
%like (: empty (-> $a)) can implement it - so the result is compatible with
%every requirement, without assigning a concrete type or emitting a guard:
set_call_out_type(Out, ATs, OT) :- ( nonvar(OT) -> set_out_type(Out, OT)
                                   ; var(Out), term_variables(ATs, Vs), \+ memberchk_eq(OT, Vs)
                                     -> add_known_type(Out, OT)
                                      ; true ).

%A call is statically dead when the same variable occupies two argument
%positions whose required types can never both hold (e.g. (num-str $x $x)):
same_call_var_conflict([V|Vs], [T|Ts]) :- ( var(V), nonvar(T), \+ wildcard_type_t(T),
                                            var_conflict_in_rest(V, T, Vs, Ts) -> true
                                          ; same_call_var_conflict(Vs, Ts) ).

var_conflict_in_rest(V, T, [V2|Vs], [T2|Ts]) :- ( V == V2, nonvar(T2), \+ wildcard_type_t(T2),
                                                  %symmetric: a conflict needs NO common inhabitant,
                                                  %not merely directional incompatibility (unions!):
                                                  \+ type_compat_soft(T, T2),
                                                  \+ type_compat_soft(T2, T) -> true
                                                ; var_conflict_in_rest(V, T, Vs, Ts) ).


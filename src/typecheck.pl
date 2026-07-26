%%%%%%%%%% Compile-time typechecking support (see AGENTS.md) %%%%%%%%%%
%
% One canonical type store, one compatibility relation (type_unify/2), and one
% type channel: attributed variables on the Prolog vars representing MeTTa vars.
%   tknown - translation-time inferred/declared type candidates of a variable
%   mreq   - runtime type constraints placed on still-unbound values by guards
% Static errors are thrown during translation (never emitted and re-scanned).

:- dynamic declared_fn_type/4.     % declared_fn_type(F, ArgTypes, OutType, Det)
:- dynamic declared_value_type/2.  % declared_value_type(Name, Type)
:- dynamic declared_newtype/2.     % declared_newtype(Name, Representation) - erased nominal types
:- dynamic strict_mode/1.

:- dynamic strict_det/1.

:- current_prolog_flag(argv, Argv),
   ( memberchk('--strict-det', Argv) -> assertz(strict_mode(true)), assertz(strict_det(true))
   ; memberchk('--strict', Argv) -> assertz(strict_mode(true)), assertz(strict_det(false))
                                  ; assertz(strict_mode(false)), assertz(strict_det(false)) ).

%Soundness oracles (see examples/soundness_matrix.sh). Three independent
%switches, each a pure ADDITION of runtime checking - none of them changes
%which programs compile, only what the compiled program verifies as it runs:
%
%  --oracle      re-emits every statically discharged certification as a
%                runtime check: clause OUTPUTS (oracle_output_check/4) and
%                call-site ARGUMENTS (oracle_arg_check/3) alike.
%  --oracle-det  counts the solutions of every committed (-[det]-> /
%                -[semidet]->) call, so a function that is declared
%                deterministic but semantically is not - zero results for det,
%                two or more for either - throws instead of quietly under- or
%                over-producing. This is what --no-det-cut cannot see:
%                clause_commit_cut/2 puts its ! at clause ENTRY, so removing it
%                only exposes CLAUSE-SELECTION alternatives, and overlapping
%                heads are already a hard static error. Every determinism hole
%                found so far lives in the BODY, where --no-det-cut is blind.
%  --no-det-cut  suppresses the determinism commit itself (kept: it is the only
%                switch that shows clause-selection alternatives directly).
:- dynamic oracle_mode/1.
:- dynamic oracle_det_mode/1.
:- dynamic suppress_det_cut/1.
:- current_prolog_flag(argv, Argv),
   ( memberchk('--oracle', Argv) -> assertz(oracle_mode(true)) ; assertz(oracle_mode(false)) ),
   ( memberchk('--oracle-det', Argv) -> assertz(oracle_det_mode(true)) ; assertz(oracle_det_mode(false)) ),
   ( memberchk('--no-det-cut', Argv) -> assertz(suppress_det_cut(true)) ; assertz(suppress_det_cut(false)) ).

%--warn-runtime-checks reports every runtime type check the compiler emits -
%implicit residual guards and explicit (the ...) ascriptions alike - so the
%user can see and eliminate them one by one. Independent of --strict (where
%implicit residuals are errors anyway, this reports the explicit remainder):
:- dynamic warn_runtime_checks/1.
:- current_prolog_flag(argv, Argv),
   ( memberchk('--warn-runtime-checks', Argv) -> assertz(warn_runtime_checks(true))
                                               ; assertz(warn_runtime_checks(false)) ).

warn_residual_check(Ctx, T) :- ( warn_runtime_checks(true)
                                 -> format(user_error, "Warning: runtime type check in ~w against ~p~n", [Ctx, T])
                                  ; true ).

%%% Arrow shapes: prefix, like every MeTTa form - (-> A B), (-[det]-> A B),
%%% (-[semidet]-> A B), (-[nondet]-> A B). Under --strict-det a plain -> is a
%%% determinism commitment: functions are deterministic unless declared
%%% -[nondet]->.
%%% Cardinality is a total order: det (exactly one) < semidet (zero or one)
%%% < nondet (any). semidet commits exactly like det - it only adds the right
%%% to fail - so it keeps the clause-entry cut and last-call optimization.
plain_arrow_det(Det) :- ( strict_det(true) -> Det = det ; Det = unspecified ).

arrow_det('->', Det) :- plain_arrow_det(Det).
arrow_det('-[det]->', det).
arrow_det('-[deterministic]->', det).
arrow_det('-[semidet]->', semidet).
arrow_det('-[semideterministic]->', semidet).
arrow_det('-[nondet]->', nondet).
arrow_det('-[nondeterministic]->', nondet).

%%% The single enumeration of the CANONICAL arrow atoms (what
%%% canonical_arrow/2 produces) and the determinism each commits to. Every
%%% site that used to spell out ('->' ; '-[det]->' ; '-[nondet]->') goes
%%% through this - adding an arrow means adding a clause here, arrow_det/2 and
%%% canonical_arrow/2, and nowhere else. `plain` is the mode-dependent -> (see
%%% plain_arrow_det/1); it is deliberately NOT the atom det, so a site can ask
%%% for an explicit commitment without accidentally matching -> :
arrow_atom_det('->', plain).
arrow_atom_det('-[det]->', det).
arrow_atom_det('-[semidet]->', semidet).
arrow_atom_det('-[nondet]->', nondet).

arrow_atom(A) :- nonvar(A), arrow_atom_det(A, _).

%The determinism level of an arrow TYPE's head, only ever read, never bound:
arrow_head_level(K, L) :- nonvar(K), K = [A|_], nonvar(A), arrow_atom_det(A, L).

%A commitment that makes the compiler emit the clause-entry cut and validate
%the clause set: det and semidet both promise at most one result:
committed_det(det).
committed_det(semidet).

fn_type_shape(Type, ArgTypes, OutType, Det) :- is_list(Type), Type = [Arrow|Xs],
                                               nonvar(Arrow), atom(Arrow), arrow_det(Arrow, Det), !,
                                               append(ArgTypes, [OutType], Xs).

%An arrow atom anywhere but the head of its expression is the abandoned infix
%syntax; rejected loudly because it would otherwise silently parse as a
%value/tuple type and drop the arrow:
infix_arrow_misuse(T) :- is_list(T), T = [_|Rest],
                         member(X, Rest), nonvar(X),
                         ( atom(X) -> arrow_det(X, _) ; infix_arrow_misuse(X) ), !.
infix_arrow_misuse(T) :- is_list(T), T = [H|_], nonvar(H), infix_arrow_misuse(H).

%Normalize nested arrow types to canonical prefix form. Nondeterministic
%arrows keep their marker so closure parameters carry the commitment:
normalize_type(T, T) :- var(T), !.
normalize_type(T, T) :- atomic(T), !.
normalize_type(T, TN) :- is_list(T), fn_type_shape(T, ATs, OT, _), !,
                         T = [Arrow|_],
                         canonical_arrow(Arrow, H),
                         maplist(normalize_type, ATs, ATN),
                         normalize_type(OT, OTN),
                         append(ATN, [OTN], Xs),
                         TN = [H|Xs].
normalize_type(T, TN) :- is_list(T), !, maplist(normalize_type, T, TN).
normalize_type(T, T).

%Explicit determinism markers survive normalization so closure parameters
%carry their commitment in every mode, not only under --strict-det:
canonical_arrow('-[det]->', '-[det]->') :- !.
canonical_arrow('-[deterministic]->', '-[det]->') :- !.
canonical_arrow('-[semidet]->', '-[semidet]->') :- !.
canonical_arrow('-[semideterministic]->', '-[semidet]->') :- !.
canonical_arrow('-[nondet]->', '-[nondet]->') :- !.
canonical_arrow('-[nondeterministic]->', '-[nondet]->') :- !.
canonical_arrow(_, (->)).

%%% Store maintenance, called from add_sexp/remove_sexp and forget_symbol.
%%% Caching is idempotent so seeded builtins and imports do not duplicate:
%A function type declared for a parenthesized name - (: (/?\) (-> ...)) - is a
%malformed declaration that would otherwise be ignored silently:
maybe_cache_type_decl(Space, Term) :- Space == '&self', is_list(Term), Term = [C, [Name], Type],
                                      C == (:), atom(Name),
                                      nonvar(Type), fn_type_shape(Type, _, _, _), !,
                                      format(user_error,
                                             "Warning: type declaration name (~w) is an expression; write (: ~w ...) to declare the function~n",
                                             [Name, Name]).
%Erased nominal newtypes: (: KB (Newtype Expression)) declares KB as a
%distinct compile-time role over the given representation. Nothing exists at
%runtime; the brand lives purely in the checker.
maybe_cache_type_decl(Space, Term) :- Space == '&self', is_list(Term), Term = [C, Name, [NT, R]],
                                      C == (:), atom(Name), NT == 'Newtype', !,
                                      normalize_type(R, RN),
                                      ( declared_newtype(Name, R2), R2 =@= RN -> true
                                                                              ; assertz(declared_newtype(Name, RN)) ).
maybe_cache_type_decl(Space, Term) :- ( Space == '&self', is_list(Term), Term = [C, Name, Type],
                                        C == (:), atom(Name)
                                        -> ( nonvar(Type), infix_arrow_misuse(Type)
                                             -> throw(error(infix_arrow_syntax(Name, Type), typecheck))
                                           ; nonvar(Type), fn_type_shape(Type, ATs, OT, Det)
                                             -> maplist(normalize_type, ATs, ATN),
                                                normalize_type(OT, OTN),
                                                note_explicit_det_decl(Name, Type, ATN),
                                                retractall(inferred_fn_type(Name, _, _)),  %declaration supersedes inference
                                                ( declared_fn_type(Name, A2, O2, D2),
                                                  (A2-O2-D2) =@= (ATN-OTN-Det) -> true
                                                ; assertz(declared_fn_type(Name, ATN, OTN, Det)),
                                                  enforce_late_declaration(Name),
                                                  note_constructor_set_change(Name) )
                                              ; normalize_type(Type, TN),
                                                ( declared_value_type(Name, T2), T2 =@= TN -> true
                                                ; assertz(declared_value_type(Name, TN)),
                                                  note_constructor_set_change(Name) ) )
                                         ; true ).

%declared_fn_type/4 keeps the determinism, not the arrow that expressed it, and
%under --strict-det a plain -> also yields det. The exhaustiveness check needs
%the difference: an EXPLICIT -[det]-> is a per-function promise of exactly one
%result, a plain -> is a mode-wide default. Only the former is recorded here:
:- dynamic explicit_det_decl/2.

note_explicit_det_decl(Name, Type, ATs) :- ( nonvar(Type), Type = [Arrow|_], atom(Arrow),
                                             canonical_arrow(Arrow, '-[det]->'),
                                             length(ATs, N), \+ explicit_det_decl(Name, N)
                                             -> assertz(explicit_det_decl(Name, N)) ; true ).

%Declaration prepass: only function (arrow) declarations are hoisted, so
%definitions may call helpers declared later in the same file. Value
%declarations stay order-sensitive - they are knowledge atoms whose position
%is meaningful (see examples/types_nondet.metta):
precache_fn_type_decl(Space, Term) :- ( is_list(Term), Term = [C, Name, Type],
                                        C == (:), atom(Name), nonvar(Type),
                                        fn_type_shape(Type, _, _, _)
                                        -> maybe_cache_type_decl(Space, Term)
                                         ; true ).

%Seed the store with the builtin operator types (called once after loading):
seed_builtin_types :- standard_library_path(Base),
                      atomic_list_concat([Base, '/lib_builtin_types.metta'], Path),
                      read_file_to_string(Path, S, []),
                      metta_string_forms(S, Forms),
                      forall(member(form(FormStr, _), Forms),
                             ( sread(FormStr, Term),
                               maybe_cache_type_decl('&self', Term) )).

maybe_uncache_type_decl(Space, Term) :- ( Space == '&self', is_list(Term), Term = [C, Name, Type],
                                          C == (:), atom(Name)
                                          -> ( nonvar(Type), fn_type_shape(Type, ATs, OT, Det)
                                               -> maplist(normalize_type, ATs, ATN),
                                                  normalize_type(OT, OTN),
                                                  ( clause(declared_fn_type(Name, A2, O2, D2), true, Ref),
                                                    (A2-O2-D2) =@= (ATN-OTN-Det)
                                                    -> erase(Ref),
                                                       length(ATN, NA),
                                                       ( declared_fn_type(Name, A3, _, _), length(A3, NA)
                                                         -> true ; retractall(explicit_det_decl(Name, NA)) )
                                                     ; true )
                                                ; normalize_type(Type, TN),
                                                  ( clause(declared_value_type(Name, T2), true, Ref),
                                                    T2 =@= TN
                                                    -> erase(Ref) ; true ) )
                                           ; true ).

%%% A declaration that arrives after the function was compiled used to be
%%% BELIEVED and never ENFORCED: the clauses were validated (and emitted) with
%%% no declaration in sight, so a late -[det]-> got no overlap check, no body
%%% determinism check and no commit cut, while every later caller was told the
%%% function is det. A warning is not enough for that - the compiler was
%%% asserting something it had not checked.
%%%
%%% This cannot happen INSIDE a file: process_metta_string/3 pre-caches every
%%% arrow declaration of a file before compiling any of its definitions. It is
%%% specifically a cross-file (or add-atom-at-runtime) situation, so recompiling
%%% is cheap - it revisits one function, not the program.
%%%
%%% Recompiling, rather than rejecting, is what a declaration prepass would
%%% have done had the two files been one, so it is the semantics that already
%%% exists rather than a new rule. The clauses go back through
%%% translate_clause/2 with the declaration in place: they get their argument
%%% and output certifications, their determinism verdict, and their commit cut,
%%% and if they cannot satisfy the declaration the normal error is thrown.
%%% The warning stays, because a recompile can change a program that already
%%% ran part of itself.
enforce_late_declaration(Name) :-
    ( catch(nb_getval(Name, [_|_]), _, fail)
      -> format(user_error,
                "Warning: type declaration for ~w arrives after its definition; its clauses are being recompiled against it~n",
                [Name]),
         recompile_function_clauses(Name)
       ; true ).

forget_symbol_types(Name) :- retractall(declared_fn_type(Name, _, _, _)),
                             retractall(explicit_det_decl(Name, _)),
                             retractall(declared_value_type(Name, _)),
                             retractall(declared_newtype(Name, _)),
                             retractall(inferred_fn_type(Name, _, _)).

%%% Store lookup (each retrieval yields a fresh copy of the declaration):
fn_decl_arity(F, N, ATs, OT) :- declared_fn_type(F, ATs, OT, _), length(ATs, N).
fn_decl_partial(F, N, PTs, RTs, OT) :- fn_decl_partial(F, N, PTs, RTs, OT, _).
fn_decl_partial(F, N, PTs, RTs, OT, Det) :- declared_fn_type(F, ATs, OT, Det),
                                            length(ATs, Total), Total > N,
                                            length(PTs, N), append(PTs, RTs, ATs).

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
                                                      ; note_unknown_candidate(Out) )
                                  ; known_candidates(Val, Cs) -> add_known_types(Out, Cs)
                                  ; note_unknown_candidate(Out) )
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

%%% Static typing of values (translated call results, literals, closures):
value_candidate_types(V, ['Number']) :- number(V), !.
value_candidate_types(V, ['String']) :- string(V), !.
value_candidate_types(true, ['Bool']) :- !.
value_candidate_types(false, ['Bool']) :- !.
value_candidate_types(V, Cs) :- atom(V), !,
                                findall(T, declared_value_type(V, T), Vs),
                                findall([H|Xs], ( declared_fn_type(V, ATs, OT, Det),
                                                  length(ATs, NA), value_arrow_head(V, NA, Det, H),
                                                  append(ATs, [OT], Xs) ), Fs),
                                append(Vs, Fs, Cs0),
                                ( Cs0 == [], current_arithmetic_function(V)
                                  -> Cs = ['Number']                %arithmetic constants: inf, nan, pi, e
                                   ; Cs = Cs0 ).
value_candidate_types(partial(F, B), Cs) :- !,
                                length(B, N),
                                findall([H|Xs], ( fn_decl_partial(F, N, PTs, RTs, OT, Det),
                                                  length(RTs, NR), NA is N + NR,
                                                  value_arrow_head(F, NA, Det, H),
                                                  bound_args_match(B, PTs),
                                                  append(RTs, [OT], Xs) ), Cs).
value_candidate_types([], [['List', _]]) :- !.
%A constructor application (STV 0.5 0.8) has the constructor's output type,
%but only when its fields do not contradict the constructor's signature -
%otherwise the value is unknown and the (runtime or strict) guard decides.
%
%is_list/1 before length/2 is load-bearing, not defensive. A head pattern
%written with a variable tail - (cons Premises $p), which constrain_args/3
%compiles to the PARTIAL list ['Premises'|$p] - reaches here, and length/2 on
%a partial list is a GENERATOR: it proposes N = 0, 1, 2, ... forever, and
%since fn_decl_arity/4 fails for each one nothing ever cuts the loop. It ran
%out to a 76-million-element term (35s, 2.2GB, stack overflow) on a two-line
%file. A term whose tail is still unbound is not an n-argument constructor
%application - its arity is not known yet - so this clause simply does not
%apply to it, and it falls through to the "no candidate types" answer, which
%is the honest one. is_list/1 is also cycle-safe, so a rational tree (an
%inferred self-referential value) fails here rather than looping.
value_candidate_types([H|Args], Cs) :- atom(H), is_list(Args), length(Args, N), fn_decl_arity(H, N, _, _), !,
                                findall(OT, ( fn_decl_arity(H, N, ATs, OT),
                                              bound_args_match(Args, ATs) ), Cs).
value_candidate_types(V, Cs) :- is_list(V), maplist(value_single_type, V, Ts), !, Cs = [Ts].
value_candidate_types(_, []).

value_single_type(V, T) :- ( var(V) -> known_singleton(V, T)
                                     ; value_candidate_types(V, [T0]), T = T0 ).

det_arrow_head(Det, H) :- nonvar(Det), arrow_atom_det(H, Det), !.
det_arrow_head(_, (->)).

%The arrow head a declared symbol carries when it is used as a VALUE. Where
%the checker's own builtin table has an entry it OVERRIDES the declaration,
%exactly as it does for a direct call (function_call_determinism/3) and for
%the oracle's wrapping decision (oracle_det_believed/3). lib_builtin_types
%declares (: or (-> Bool Bool Bool)); plain_arrow_det/1 reads that plain arrow
%as a det commitment under --strict-det, so without this the same symbol was
%det as a closure argument and nondet as a call. An undeclared builtin already
%got this right, through inferred_arrow_head/3 - the declaration was the only
%thing hiding the table:
value_arrow_head(F, N, Det, H) :- ( atom(F), builtin_call_determinism(F, N, DetB)
                                    -> det_arrow_head(DetB, H)
                                     ; det_arrow_head(Det, H) ).

bound_args_match(B, PTs) :- \+ \+ maplist(arg_soft_ok, B, PTs).

%%% check_value(+Value, ?Type, -Status): Status in {ok, mismatch, unknown}.
%%% Binds type variables in Type on success (polymorphism resolution).
%%% Primitive fast paths first: they carry the hot arithmetic call sites.
check_value(V, T, St) :- number(V), !, ( var(T) -> T = 'Number', St = ok
                                       ; T == 'Number' -> St = ok
                                       ; prim_mismatch_status('Number', T, St) ).
check_value(V, T, St) :- string(V), !, ( var(T) -> T = 'String', St = ok
                                       ; T == 'String' -> St = ok
                                       ; prim_mismatch_status('String', T, St) ).
check_value(V, T, St) :- ( V == true ; V == false ), !,
                         ( var(T) -> T = 'Bool', St = ok
                         ; T == 'Bool' -> St = ok
                         ; prim_mismatch_status('Bool', T, St) ).
check_value(V, T, St) :- var(T), !, ( value_single_type(V, VT) -> T = VT ; true ), St = ok.
check_value(_, T, St) :- wildcard_type_t(T), !, St = ok.
check_value(V, T, St) :- is_union(T), !, T = ['|'|Ms],
                         ( member(M, Ms), check_value(V, M, SM), SM == ok -> St = ok
                         ; forall(member(M, Ms), check_value(V, M, mismatch)) -> St = mismatch
                         ; St = unknown ).
check_value(V, T, St) :- list_type(T, ET), !,
                         ( is_list(V) -> list_elems_status(V, ET, St)
                         ; non_list(V) -> St = mismatch
                         ; St = unknown ).
check_value(V, T, St) :- is_arrow_type(T), !,
                         ( ( atom(V) ; V = partial(_, _) )
                           -> value_candidate_types(V, Cs),
                              ( Cs == [] -> ( inferred_value_candidates(V, ICs),
                                              member(C, ICs), type_unify(C, T)
                                              -> St = ok       %inferred types are positive evidence only
                                               ; St = unknown )
                              ; member(C, Cs), type_unify(C, T) -> St = ok
                              ; St = mismatch )
                         ; ( number(V) ; string(V) ) -> St = mismatch
                         ; St = unknown ).

%Structural tuple types (Tag T1 ... Tn): the value must carry the same tag
%and arity, and its fields check recursively. A primitive or wildcard atom in
%head position is a type, not a tag - ($a Number) unified to (Number Number)
%is an untagged pair, handled by the next clause. See tagged_tuple_type/3 for
%when a head atom is a tag and when it is the first field's type:
check_value(V, T, St) :- tagged_tuple_type(T, Tag, FieldTs), !,
                         ( is_list(V) -> ( V = [VTag|Fields], VTag == Tag, same_length(Fields, FieldTs)
                                           -> tuple_fields_status(Fields, FieldTs, St)
                                            ; St = mismatch )
                         ; atom(V) -> atom_value_status(V, T, St)
                         ; non_list(V) -> St = mismatch
                         ; St = unknown ).
%Untagged tuple types like ($v Number): element-wise, the head position may
%be a type variable:
check_value(V, T, St) :- is_list(T), !,
                         ( is_list(V) -> ( same_length(V, T) -> tuple_fields_status(V, T, St)
                                                              ; St = mismatch )
                         ; atom(V) -> atom_value_status(V, T, St)
                         ; non_list(V) -> St = mismatch
                         ; St = unknown ).
%A raw or representation-typed value acquires a newtype contextually; a
%value already carrying a different brand does not (that is the feature):
check_value(V, T, St) :- atom(T), declared_newtype(T, R), !,
                         value_candidate_types(V, Cs),
                         ( Cs == [] -> ( constructed_definite_mismatch(V) -> St = mismatch
                                                                           ; check_value(V, R, St) )
                         ; member(C, Cs), type_unify(C, T) -> St = ok
                         ; member(C, Cs), \+ ( atom(C), declared_newtype(C, _) ),
                           type_unify(C, R) -> St = ok
                         ; St = mismatch ).
check_value(V, T, St) :- atom(T), !,
                         value_candidate_types(V, Cs),
                         ( Cs == [] -> ( constructed_definite_mismatch(V) -> St = mismatch
                                                                           ; St = unknown )
                         ; member(C, Cs), type_unify(C, T) -> St = ok
                         ; member(C, Cs), refinement_pair(C, T) -> St = unknown
                         ; St = mismatch ).
check_value(_, _, unknown).

%A constructor application every declaration of which is definitely
%contradicted by some field can never have any type. In particular a field
%branded with a different newtype is unfixable at runtime - brands are
%erased - so it must reject at compile time, not degrade to a guard:
constructed_definite_mismatch(V) :- is_list(V), V = [H|Args], atom(H),
                                    length(Args, N), fn_decl_arity(H, N, _, _),
                                    forall(fn_decl_arity(H, N, ATs, _),
                                           \+ \+ tuple_fields_status(Args, ATs, mismatch)).

%How an atom's declared candidate types stand against a required type:
atom_value_status(V, T, St) :- value_candidate_types(V, Cs),
                               ( Cs == [] -> St = unknown
                               ; member(C, Cs), type_unify(C, T) -> St = ok
                               ; St = mismatch ).

tuple_fields_status([], [], ok).
tuple_fields_status([F|Fs], [T|Ts], St) :- elem_status(F, T, S1),
                                           ( S1 == mismatch -> St = mismatch
                                           ; tuple_fields_status(Fs, Ts, S2),
                                             ( S2 == mismatch -> St = mismatch
                                             ; S1 == unknown -> St = unknown
                                             ; St = S2 ) ).

%Arrow types of closures over inferred (undeclared) functions:
%Inference makes no determinism claim by itself, but the clause-set analysis
%(the same transitive-evidence rule calls use) may PROVE one. That proof is
%worth exactly as much in every mode - --strict-det exists to force a
%determinism claim out of you, not to be a precondition for checking one you
%already wrote - so it runs unconditionally and, when it commits to det or
%semidet, the inferred arrow carries that real head. A committed head fits
%every slot a plain -> fits (see det_level_fits/2), so this only ever admits
%more. With no committed proof the old behaviour stands: conservatively
%nondet under --strict-det, an uncommitted plain -> otherwise.
committed_determinism(det).
committed_determinism(semidet).

inferred_arrow_head(F, N, H) :-
    ( catch(( body_determinism(F, N, D), committed_determinism(D) ), _, fail)
      -> det_arrow_head(D, H)
    ; strict_det(true) -> det_arrow_head(nondet, H)
    ; H = (->) ).

inferred_value_candidates(V, Cs) :- atom(V), !,
                                    findall([H|Xs], ( inferred_fn_type(V, ATs, OT),
                                                      length(ATs, N),
                                                      inferred_arrow_head(V, N, H),
                                                      append(ATs, [OT], Xs) ), Cs).
inferred_value_candidates(partial(F, B), Cs) :- !,
                                    length(B, N),
                                    findall([H|Xs], ( inferred_fn_type(F, ATs, OT),
                                                      length(ATs, Total), Total > N,
                                                      inferred_arrow_head(F, Total, H),
                                                      length(PTs, N), append(PTs, RTs, ATs),
                                                      bound_args_match(B, PTs),
                                                      append(RTs, [OT], Xs) ), Cs).
inferred_value_candidates(_, []).

%Slow completion of the primitive fast paths above:
prim_mismatch_status(P, T, St) :- ( wildcard_type_t(T) -> St = ok
                                  ; is_union(T) -> ( T = ['|'|Ms], member(M, Ms), type_compat_soft(P, M)
                                                     -> St = ok ; St = mismatch )
                                  ; atom(T), declared_newtype(T, R) -> prim_mismatch_status(P, R, St)
                                  ; atom(T) -> ( refinement_pair(P, T) -> St = unknown
                                                                        ; St = mismatch )
                                  ; ( T = [L|_], L == 'List' ; is_arrow_type(T) ) -> St = mismatch
                                  ; St = unknown ).

%A primitive/tuple type against a user-defined atom type may be a runtime
%refinement, but only once get-type has actually been extended by user code:
refinement_pair(C, T) :- user_extended_get_type,
                         ( ( primitive_type(C) ; tuple_type(C) ), user_atom_type(T) -> true
                         ; user_atom_type(C), ( primitive_type(T) ; tuple_type(T) ) ).

user_extended_get_type :- predicate_property('get-type'(_, _), number_of_clauses(N)), N > 1.

primitive_type('Number').
primitive_type('String').
primitive_type('Bool').
user_atom_type(T) :- atom(T), \+ primitive_type(T), \+ wildcard_type(T).
tuple_type(C) :- is_list(C), C \= [->|_], C \= ['List', _].

%%% Tagged vs positional structural tuple types.
%
% A type of shape (H T1 ... Tn) is read in one of two ways, and the reading is
% driven by H's DECLARATION rather than by "H looks like a name":
%
%   TAGGED - the value must be the expression (H V1 ... Vn), carrying the
%   literal atom H, with Vi : Ti. Chosen when H is a declared constructor of
%   exactly n arguments - fn_decl_arity(H, n, _, _), the same discipline
%   structural_pattern_fields/4 uses - or when H carries NO type declaration
%   at all (an anonymous structural tag: (Stats Number Number Number)).
%
%   POSITIONAL - the value is any n+1 element expression whose i-th element
%   has the i-th listed type, H included. Chosen when the head is not an atom
%   (($v Number)), is a primitive or wildcard type ((Number Number)), or is an
%   atom DECLARED as something other than an n-ary constructor - typically a
%   type name, (: Statement Type). Naming a declared type in head position can
%   only mean "field 1 has this type", so (Statement KBContext Proof TV) is a
%   4-field record, not a tuple tagged with the atom Statement.
%
% Consequence for users: declare the field types of a positional tuple. An
% undeclared head atom keeps the legacy tagged reading.
tagged_tuple_type(T, Tag, FieldTs) :- nonvar(T), T = [Tag|FieldTs],
                                      atom(Tag), user_atom_type(Tag),
                                      \+ is_arrow_type(T), \+ is_union(T), \+ list_type(T, _),
                                      length(FieldTs, N),
                                      ( fn_decl_arity(Tag, N, _, _) -> true
                                                                     ; \+ type_name_declared(Tag) ).

type_name_declared(Tag) :- ( declared_value_type(Tag, _) -> true
                           ; declared_newtype(Tag, _) -> true
                           ; declared_fn_type(Tag, _, _, _) ).

%With an unresolved element type variable, a heterogeneous list is legal: the
%element type resolves to the common element type, or stays unconstrained.
list_elems_status(Es, ET, St) :- var(ET), !, St = ok,
                                 ( maplist(value_single_type, Es, Ts), Ts = [T1|Rest],
                                   forall(member(T2, Rest), T2 =@= T1)
                                   -> ET = T1 ; true ).
list_elems_status([], _, ok).
list_elems_status([E|Es], ET, St) :- elem_status(E, ET, S1),
                                     ( S1 == mismatch -> St = mismatch
                                     ; list_elems_status(Es, ET, S2),
                                       ( S2 == mismatch -> St = mismatch
                                       ; S1 == unknown -> St = unknown
                                       ; St = S2 ) ).

elem_status(E, ET, St) :- ( var(E) -> ( known_singleton(E, K) -> ( type_unify(K, ET) -> St = ok
                                                                                      ; St = mismatch )
                                                               ; St = unknown )
                                    ; check_value(E, ET, St) ).

%%% Side-effect-free per-argument admissibility (overload filtering):
arg_soft_ok(AV, T) :- ( var(AV) -> ( known_singleton(AV, K) -> copy_term(K, K2), type_unify(K2, T)
                                                             ; true )
                                 ; check_value(AV, T, St), St \== mismatch ).

decl_survives(AVs, ft(ATs, _)) :- \+ \+ maplist(arg_soft_ok, AVs, ATs).

arg_statically_ok(AV, T) :- \+ \+ ( var(AV) -> ( known_singleton(AV, K) -> type_unify(K, T)
                                               ; ( var(T) -> true ; wildcard_type_t(T) ) )
                                             ; check_value(AV, T, ok) ).

%%% Effectful call-site argument checking, one arg.
%
% The Mode is the PROVENANCE of the required type, and it decides what failing
% to establish that type at the call site may cost:
%
%   declared - the type is a promise the author wrote down, so it is a
%   requirement: a static mismatch is a compile error and anything unresolved
%   becomes a runtime guard.
%
%   inferred - the type was reconstructed from how the callee's body happens
%   to USE the parameter, which is not the same thing as what the callee
%   REQUIRES of it. In
%
%       (= (score $current $cand) (if (== $current none) $cand (max $cand $current)))
%
%   $current is inferred Number from the else branch, but the function
%   explicitly handles none and (score none 0.42) is a correct program. A
%   requirement is therefore only imposed where the compiler can see the value
%   is definitely of an incompatible type; where it merely cannot tell - an
%   undeclared atom like none, an untyped variable - inference stays silent
%   rather than demanding a type the callee never asked for. This is the
%   README's contract: inferred types add knowledge, they do not reject
%   programs that would otherwise run.
%
%   The definite-conflict guard is kept deliberately. It is what still catches
%   (f "a") against an inferred (= (f $x) (+ $x 1)): inference ELIDED the
%   guard inside f's body, so with no check at all that call quietly computes
%   98 (SWI reads a one-character string as its character code) instead of
%   raising a type error. Dropping a false rejection must not buy a silent
%   wrong answer.
check_call_arg(Mode, Fun, AV, T, Gs) :- ( var(AV)
                                          -> ( known_singleton(AV, K)
                                               -> ( nonvar(T), wildcard_type_t(T) -> Gs = []  %wildcards carry no knowledge
                                                  ; type_unify(K, T) -> oracle_arg_check(AV, T, Gs)
                                                  %conflicting brands cannot be deferred to a runtime
                                                  %guard - newtypes are erased there - so they reject
                                                  %now, but only on a promised type:
                                                  ; atom(T), declared_newtype(T, _), atom(K), declared_newtype(K, _)
                                                    -> ( Mode == declared
                                                         -> throw(error(type_conflict(existing(K), required(T)), typecheck))
                                                          ; taint_assumption(AV), Gs = [] )
                                                  ; taint_assumption(AV),  %known conflict: runtime error carries the value
                                                    type_guard(Fun, AV, T, Gs) )
                                             ; var(T) -> Gs = []
                                             ; wildcard_type_t(T) -> Gs = []
                                             %an untyped value is not evidence of a wrong one:
                                             ; Mode == inferred -> Gs = []
                                             ; type_guard(Fun, AV, T, Gs) )
                                        ; check_value(AV, T, St),   %also binds an open T: knowledge
                                          ( St == ok -> oracle_arg_check(AV, T, Gs)
                                          ; St == mismatch
                                            -> ( Mode == declared
                                                 -> throw(error(literal_type_mismatch(AV, T), typecheck))
                                                  ; type_guard(Fun, AV, T, Gs) )
                                          ; Mode == inferred -> Gs = []
                                          ; type_guard(Fun, AV, T, Gs) ) ).

%Open structured types (e.g. (List $a)) still guard their outer shape; only a
%fully unconstrained type variable needs no check at all:
type_guard(Fun, AV, T, Gs) :- ( nonvar(T), \+ wildcard_type_t(T)
                                -> ( undecidable_arrow_commitment(T)
                                     -> throw(error(determinism_conflict(Fun, unproven_closure(AV, T)), determinism))
                                   ; strict_mode(true)
                                     -> throw(error(strict_runtime_typecheck(Fun, typecheck_or_error(AV, T)), typecheck))
                                      ; warn_residual_check(Fun, T),
                                        guard_goal(AV, T, G), Gs = [G] )
                                 ; Gs = [] ).

%A runtime type check cannot count a closure's solutions: nothing it can
%inspect distinguishes a det function from a nondet one. So a determinism
%COMMITMENT in a required arrow type is undischargeable at runtime and must
%not be deferred to a guard - the same reason a conflicting newtype brand
%rejects rather than guards (brands are erased at runtime, determinism was
%never there in the first place). Reaching type_guard/4 with such a type means
%the commitment could not be established statically, so it is rejected here.
%A plain -> claims nothing unless --strict-det makes it a commitment:
undecidable_arrow_commitment(T) :- is_arrow_type(T), T = [H|_], arrow_atom_det(H, L),
                                   ( committed_det(L) -> true
                                   ; L == plain -> strict_det(true)
                                   ; fail ).

%Inline the primitive fast path into the compiled goal so hot code only pays a
%native type test; the reflective check runs only when that test fails:
guard_goal(AV, 'Number', ( number(AV) -> true ; typecheck_or_error(AV, 'Number') )) :- !.
guard_goal(AV, 'String', ( string(AV) -> true ; typecheck_or_error(AV, 'String') )) :- !.
guard_goal(AV, 'Bool', ( ( AV == true ; AV == false ) -> true ; typecheck_or_error(AV, 'Bool') )) :- !.
guard_goal(AV, T, typecheck_or_error(AV, T)).

%Compiling a call to `fail` is a rejection too - a silent one - so it also
%needs a type the author actually promised (see check_call_arg/5):
apply_call_args(Mode, Fun, AVs, ATs, Gs) :- ( Mode == declared, same_call_var_conflict(AVs, ATs) -> Gs = [fail]
                                            ; maplist(check_call_arg(Mode, Fun), AVs, ATs, Gss),
                                              append(Gss, Gs) ).

%%% Runtime residual guards (only emitted where types stay unresolved).
%%% Bound values are checked via the user-extensible get-type reflection, so
%%% runtime refinement types (see examples/types_dependent.metta) keep working:
typecheck_or_error(V, T) :- ( var(V) -> constrain_var_type(V, T)
                            ; runtime_type_ok(V, T) -> true
                            ; throw(error(literal_type_mismatch(V, T), typecheck)) ).

%Non-throwing variant used inside overload dispatch branches:
typecheck_match(V, T) :- ( var(V) -> constrain_var_type(V, T)
                                   ; runtime_type_ok(V, T) ).

%Fast paths first: primitive values in hot code must not pay for reflection.
runtime_type_ok(V, 'Number') :- number(V), !.
runtime_type_ok(V, 'String') :- string(V), !.
runtime_type_ok(V, 'Bool') :- ( V == true ; V == false ), !.
runtime_type_ok(_, T) :- var(T), !.
runtime_type_ok(_, T) :- wildcard_type_t(T), !.
runtime_type_ok(V, T) :- list_type(T, ET), !,
                         is_list(V),
                         runtime_list_ok(V, ET).
runtime_type_ok(V, T) :- is_arrow_type(T), !, \+ value_definitely_mismatch(V, T).
runtime_type_ok(V, T) :- is_union(T), !, T = ['|'|Ms],
                         member(M, Ms), runtime_type_ok(V, M), !.
%Newtypes are erased: at runtime only the representation exists.
runtime_type_ok(V, T) :- atom(T), declared_newtype(T, R), !, runtime_type_ok(V, R).
runtime_type_ok(V, T) :- tagged_tuple_type(T, Tag, FieldTs), !,
                         is_list(V), V = [VTag|Fields], VTag == Tag,
                         same_length(Fields, FieldTs),
                         runtime_tuple_ok(Fields, FieldTs).
runtime_type_ok(V, T) :- is_list(T), !,
                         is_list(V), same_length(V, T),
                         runtime_tuple_ok(V, T).
%Nominal values type by their cached declarations before falling back to
%get-type reflection (which scans &self per lookup - hot residual checks on
%constructed values must not pay that). Only positive matches commit:
runtime_type_ok(V, T) :- atom(T), nominal_value_ok(V, T), !.
%get-type is user-extensible and extensions may call typechecked code, so a
%guard reached from within a get-type call must not recurse into get-type:
runtime_type_ok(_, _) :- nb_current('$in_typecheck', true), !.
runtime_type_ok(V, T) :- setup_call_cleanup(nb_setval('$in_typecheck', true),
                                            ( 'get-type'(V, T) *-> true ; 'get-metatype'(V, T) ),
                                            nb_setval('$in_typecheck', false)).

runtime_list_ok([], _).
runtime_list_ok([E|Es], ET) :- ( var(E) -> true ; runtime_type_ok(E, ET) ),
                               runtime_list_ok(Es, ET).

%A constructor application whose unique declaration outputs T (fields still
%checked), or a value atom declared T:
nominal_value_ok(V, T) :- is_list(V), V = [Ctor|Fields], atom(Ctor), !,
                          length(Fields, N),
                          findall(ATs-OT, fn_decl_arity(Ctor, N, ATs, OT), [FieldTs-OT1]),
                          OT1 == T,
                          runtime_tuple_ok(Fields, FieldTs).
nominal_value_ok(V, T) :- atom(V), declared_value_type(V, VT), VT == T.

runtime_tuple_ok([], []).
runtime_tuple_ok([F|Fs], [T|Ts]) :- ( var(F) -> true ; runtime_type_ok(F, T) ),
                                    runtime_tuple_ok(Fs, Ts).

constrain_var_type(V, T) :- ( get_attr(V, mreq, Rs)
                              -> ( member(R, Rs), \+ type_compat_soft(R, T) -> fail
                                 %ground duplicates add nothing; nonground variants are NOT
                                 %duplicates - their type vars can be bound independently:
                                 ; ground(T), memberchk(T, Rs) -> true
                                 ; put_attr(V, mreq, [T|Rs]) )
                               ; put_attr(V, mreq, [T]) ).

value_definitely_mismatch(V, T) :- copy_term(T, T2), check_value(V, T2, St), !, St == mismatch.

goal_or_throw(Goal, Error) :- ( call(Goal) *-> true ; throw(Error) ).

%%% Clause-level helpers %%%

%Bind declared parameter types onto clause-head variables. For an overloaded
%function, the clause's head patterns filter the declarations: a clause whose
%head selects exactly one overload is checked against it, a clause no overload
%can produce is rejected, and a genuinely ambiguous clause (e.g. all-variable
%head serving every overload) stays unchecked as before.
clause_param_types(F, Args, DeclOut) :- length(Args, N),
                                        findall(ATs-OTx, fn_decl_arity(F, N, ATs, OTx), Decls),
                                        ( Decls == [] -> DeclOut = none
                                        ; Decls = [ATs1-OT] -> maplist(bind_param_type, Args, ATs1), DeclOut = out(OT, ATs1)
                                        ; include(clause_head_survives(Args), Decls, Survivors),
                                          ( Survivors == [] -> throw(error(no_matching_overload(F), typecheck))
                                          ; Survivors = [ATs1-OT] -> maplist(bind_param_type, Args, ATs1),
                                                                     DeclOut = out(OT, ATs1)
                                          ; DeclOut = none ) ).

clause_head_survives(Args, ATs-_) :- \+ \+ maplist(head_arg_soft, Args, ATs).
head_arg_soft(A, T) :- ( var(A) -> true
                       ; check_value(A, T, St) -> St \== mismatch
                       ; true ).

bind_param_type(Arg, T) :- ( var(Arg) -> ( nonvar(T) -> ( \+ wildcard_type_t(T) -> add_known_type(Arg, T)
                                                                                  ; true )
                                           %a variable type is the declaration instance: recording it
                                           %lets identical unknowns be recognized (e.g. rcons's $a):
                                           ; add_known_type(Arg, T) )
                           ; list_type(T, ET), Arg = [H|Rest]
                             -> bind_param_type(H, ET),          %type element vars of list patterns
                                bind_param_type(Rest, ['List', ET])
                           ; is_union(T)                        %clause heads narrow union params
                             -> bind_pattern_typed(Arg, T)
                           ; structural_pattern_fields(Arg, T, Fields, FieldTs)
                             -> maplist(bind_param_type, Fields, FieldTs)
                           ; is_list(Arg), is_list(T), same_length(Arg, T),
                             \+ is_arrow_type(T)                 %untagged tuple types: ($v Number)
                             -> maplist(bind_param_type, Arg, T)
                           ; check_value(Arg, T, St),
                             ( St == mismatch -> throw(error(literal_type_mismatch(Arg, T), typecheck))
                                               ; true ) ).

%Which union member does a pattern's shape select?
pattern_selects_member(P, M) :- nonvar(M), nonvar(P),
                                ( list_type(M, _) -> ( P == [] ; P = [C|_], C == cons ; is_list(P) )
                                ; atom(M) -> \+ \+ structural_pattern_fields(P, M, _, _)
                                ; is_list(M), is_list(P)
                                  -> ( tagged_tuple_type(M, Tag, FTs)
                                       -> P = [Tag2|Fs], Tag2 == Tag, same_length(Fs, FTs)
                                        ; same_length(P, M) )
                                ; fail ).

%Tag evidence outranks shape: (box $pair) also parses as a plain list, but a
%head atom that is a declared constructor of (or the tag of) exactly one
%member makes that member the selection - nominal tags are the idiomatic
%union discriminator (see strict_tuple_types.metta):
pattern_selects_member_tagged(P, M) :- nonvar(M), nonvar(P), P = [Tag|Fs], atom(Tag),
                                       ( atom(M) -> \+ \+ structural_pattern_fields(P, M, _, _)
                                       ; tagged_tuple_type(M, Tag2, FTs), Tag2 == Tag,
                                         same_length(Fs, FTs) ).

%A tagged pattern (Tag P1 ... Pn) against either the structural tuple type
%(Tag T1 ... Tn) or a nominal type produced by Tag's constructor declaration:
structural_pattern_fields(Arg, T, Fields, FieldTs) :- is_list(Arg), Arg = [Tag|Fields], atom(Tag), nonvar(T),
                                                      ( tagged_tuple_type(T, Tag2, FieldTs), Tag2 == Tag,
                                                        same_length(Fields, FieldTs) -> true
                                                      ; atom(T), length(Fields, N),
                                                        findall(ATs-OT, fn_decl_arity(Tag, N, ATs, OT), [FieldTs-OT1]),
                                                        type_compat_soft(OT1, T) ).

%Contextual output typing for deliberately-undeclared builtins (one clause per
%builtin; the translator consults this after translating an undeclared call):
untyped_call_out(cons, [H, Tl], Out) :- cons_out_type(H, Tl, Out).
untyped_call_out('cons-atom', [H, Tl], Out) :- cons_out_type(H, Tl, Out).
untyped_call_out('union-atom', [A, B], Out) :- union_atom_out_type(A, B, Out).
untyped_call_out(append, [A, B], Out) :- union_atom_out_type(A, B, Out).
untyped_call_out('subtraction-atom', [A, _], Out) :- first_list_out_type(A, Out).
untyped_call_out(list_to_set, [A], Out) :- first_list_out_type(A, Out).

%Element-filtering builtins preserve their first argument's list type; the
%other operand may be any expression:
first_list_out_type(A, Out) :- ( var(Out), union_side_elem(A, T)
                                 -> set_out_type(Out, ['List', T]) ; true ).

%cons stays undeclared (a global (List $a) signature would reject legal
%heterogeneous expressions), but when the head provably fits the tail's list
%type the result is known to be that list type:
cons_out_type(H, Tl, Out) :- ( var(Out),
                               ( var(Tl) -> known_singleton(Tl, TT), list_type(TT, T)
                               ; Tl == [] -> true
                               ; list_elem_type(Tl, T) ),
                               ( wildcard_type_t(T) -> true    %(List %Undefined%): any head fits
                               ; var(H) -> known_singleton(H, K), type_unify(K, T)
                                         ; check_value(H, T, St), St == ok )
                               -> set_out_type(Out, ['List', T])
                                ; true ).

%union-atom likewise stays undeclared, but concatenating two provably
%compatible lists yields that list type:
union_atom_out_type(A, B, Out) :- ( var(Out),
                                    union_side_elem(A, TA),
                                    union_side_elem(B, TB),
                                    type_unify(TA, TB)
                                    -> set_out_type(Out, ['List', TA])
                                     ; true ).

union_side_elem(X, T) :- ( var(X) -> known_singleton(X, K), list_type(K, T)
                         ; X == [] -> true
                         ; list_elem_type(X, T) ).

%Destructuring bindings: type a pattern's variables from the bound value's
%known type, e.g. (let (Stats $sum $sq $n) (make-stats) ...). With no type for
%the value, a pattern headed by a uniquely declared constructor still knows
%what its own fields are - that is the constructor's declaration talking, not
%the scrutinee's:
bind_pattern_from(Pat, Val) :- ( nonvar(Pat)
                                 -> ( ( var(Val) -> known_singleton(Val, KT)
                                                  ; value_single_type(Val, KT) ),
                                      nonvar(KT)                %an open assumption type says nothing yet
                                      -> bind_pattern_typed(Pat, KT)
                                       ; ctor_pattern_field_types(Pat) )
                                  ; true ).

%Tolerant variant used where a non-matching pattern must not fail or throw
%(case branches: a wrong pattern just never matches at runtime):
bind_pattern_typed(P, T) :- bind_pattern_typed(P, T, []).

%bind_pattern_typed(+Pattern, +Type, +PriorPatterns). PriorPatterns are the
%patterns of EARLIER branches of the same case, in source order, and are empty
%for every other caller (clause heads, let destructuring, meta typing) - those
%are not first-match. They are consulted only for the top-level pattern; field
%patterns recurse with [], since what an earlier branch matched at the top says
%nothing about a nested field.
bind_pattern_typed(P, T, Prior) :-
                            ( var(P) -> ( nonvar(T), \+ wildcard_type_t(T) -> add_known_type(P, T) ; true )
                            ; is_union(T), T = ['|'|Ms]        %a pattern narrows to the member it selects
                              -> ( findall(M, ( member(M, Ms), pattern_selects_member(P, M) ), [M1]),
                                   narrowing_sound(P, Ms, M1, Prior)
                                   -> bind_pattern_typed(P, M1, Prior)
                                 ; findall(M, ( member(M, Ms), pattern_selects_member_tagged(P, M) ), [M2])
                                   -> bind_pattern_typed(P, M2, Prior) ; true )
                            ; list_type(T, ET), P = [C, H, R], C == cons
                              -> bind_pattern_typed(H, ET),    %source-form (cons H R) destructuring
                                 bind_pattern_typed(R, ['List', ET])
                            ; list_type(T, ET), P = [H|Rest]
                              -> bind_pattern_typed(H, ET),
                                 bind_pattern_typed(Rest, ['List', ET])
                            ; structural_pattern_fields(P, T, Fields, FieldTs)
                              -> maplist(bind_pattern_typed, Fields, FieldTs)
                            ; is_list(P), is_list(T), same_length(P, T),
                              \+ is_arrow_type(T)
                              -> maplist(bind_pattern_typed, P, T)
                            ; true ).

%%% Soundness gate on union narrowing by shape.
%
%A pattern that carries TAG evidence for the member it selected - its head is
%that member's tag, or a declared constructor of it - says something real
%about the value, and narrows as it always has.
%
%Without that evidence (a variable head: ($_type ($_kbid $_ctx $_vars) $prf
%$tv), or an atom head that is nobody's constructor) the pattern selected the
%member purely by element count, which alone is unsound: another member's
%constructor may build a value of exactly that count (a Goal is
%(CPU $f $a $r), also four elements). Narrowing is then admissible only when
%every OTHER member is ruled out - see union_member_excluded/3.
narrowing_sound(P, _, M1, _) :- pattern_selects_member_tagged(P, M1), !.
narrowing_sound(P, Ms, _, Prior) :- is_list(P), length(P, N),
                                    forall( ( member(M, Ms), \+ pattern_selects_member(P, M) ),
                                            union_member_excluded(M, N, Prior) ).

%union_member_excluded(+Member, +N, +PriorPatterns): no value of Member can be
%an N-element expression here. Either
%  (a) by ARITY - Member has no constructor that builds N elements, or
%  (b) because every constructor of Member that does was already consumed by an
%      EARLIER branch of the same case. case is first-match/committed
%      (translate_case compiles to nested if-then-else), so such a value can
%      never reach this branch.
%(a) reads the constructor set as it stands right now, so the verdict is a
%SNAPSHOT. It used to be a standing limitation - a constructor for Member
%declared in a later file invalidated an exclusion already made and nothing
%revisited the clause. The snapshot is now recorded (note_ctor_snapshot/1) and
%a later declaration that changes the set recompiles the clauses that read it;
%see "Constructor-set snapshots" below.
union_member_excluded(M, _, _) :- var(M), !, fail.
union_member_excluded(M, _, _) :- is_arrow_type(M), !.       %a closure is not an expression
union_member_excluded(M, _, _) :- list_type(M, _), !, fail.  %(List T) admits every length
union_member_excluded(M, N, Prior) :- is_union(M), !, M = ['|'|Ms],
                                      forall(member(M2, Ms), union_member_excluded(M2, N, Prior)).
union_member_excluded(M, N, Prior) :- is_list(M), !,
        ( tagged_tuple_type(M, Tag, FieldTs)
          -> ( length(M, N) -> length(FieldTs, K), prior_consumed_ctor(Prior, Tag, K) ; true )
           ; length(M, LM), LM =\= N ).      %positional member of a different width
union_member_excluded(M, N, Prior) :- atom(M), !,
        ( wildcard_type(M) -> fail           %Atom/Expression admit anything
        ; declared_newtype(M, R) -> union_member_excluded(R, N, Prior)
        ; primitive_type(M) -> true          %an expression is not a Number/String/Bool
        ; N =:= 0 -> true                    %() is no constructor application
        ; K is N - 1,
          note_ctor_snapshot(M),             %this verdict depends on M's constructor set
          forall(member_ctor(M, K, C), prior_consumed_ctor(Prior, C, K)) ).
union_member_excluded(_, _, _) :- fail.

%A CONSTRUCTOR of the nominal type M taking K arguments, so its applications
%have K+1 elements. PeTTa's constructor convention is the one
%declared_undefined_atom/2 (translator.pl) already implements: a declared
%symbol with NO equations stays literal data, one with equations is always
%rewritten at the call site and never survives as a value. So a declaration
%alone is not enough - \+ fun(C) is what makes C data. Counting reducible
%helpers here would only ever BLOCK an exclusion, never grant a wrong one, but
%it blocks far too much: any (= (make-goal $f $a $r) (CPU $f $a $r)) would
%stop CPU/3 from being Goal's only constructor. A wildcard output claims
%nothing, so it does not block either.
%The definedness flag is set early enough: parse_form/2 (filereader.pl)
%register_fun's every (= (F ...) ...) of a file in the parse prepass, before
%any clause of that file is compiled, so definition-below-use is fine. A
%definition arriving from a LATER file only ever unblocks an exclusion that
%was conservatively refused, and recompile_late_uses/1 revisits the clauses
%that saw the symbol as data.
member_ctor(M, K, C) :- declared_fn_type(C, ATs, OT, _), length(ATs, K),
                        \+ fun(C),
                        nonvar(OT), \+ wildcard_type_t(OT), type_compat_soft(OT, M).

%%% Constructor-set snapshots %%%
%%%
%%% Two static verdicts are read off the constructor set of a nominal type:
%%% union_member_excluded/3 (this member cannot build an N-element value) and
%%% domain_keys/3 (these are all the value shapes of this type, used by the
%%% exhaustiveness checks). PeTTa's types are OPEN - a constructor may be
%%% declared in any later file - so both verdicts are snapshots, and a
%%% constructor arriving afterwards silently invalidates them.
%%%
%%% Every snapshot is therefore recorded with the key set it saw, and every
%%% new type declaration re-reads them. A set that changed means some clause
%%% was compiled on a premise that no longer holds:
%%%
%%%   - a clause verdict (union narrowing, case coverage) is REDONE - the
%%%     clause goes back through translate_clause/2 with the new constructor
%%%     set, exactly as if the constructor had been declared first. It shares
%%%     recompile_function_clauses/1 with the late-declaration fix, which is
%%%     the same problem in a different disguise.
%%%   - an exhaustiveness verdict has no clause to redo (it is a property of
%%%     the whole clause set, judged in a per-file prepass), so it is simply
%%%     re-run and throws if the function is no longer exhaustive.
%%%
%%% Both are gated behind "nothing was recorded", so a program that never
%%% narrows a union and declares no -[det]-> pays nothing.
:- dynamic ctor_snapshot_use/3.        % ctor_snapshot_use(Type, KeySnapshot, Function)
:- dynamic det_exhaustive_verdict/8.   % det_exhaustive_verdict(F, N, Heads, Consts, Types, File, Line, FormStr)

%The accumulator is only open while a clause is being translated; outside one
%(the exhaustiveness prepass, say) there is no clause to attribute a snapshot
%to and the verdict is recorded by other means:
ctor_deps(Ds) :- catch(nb_getval('$ctor_deps', Ds), _, Ds = none).

note_ctor_snapshot(T) :- ctor_deps(Ds),
                         ( Ds == none -> true
                         ; memberchk(T, Ds) -> true
                         ; nb_setval('$ctor_deps', [T|Ds]) ).

%Opens the accumulator for one clause translation and records what it read.
%Nests safely (the specializer re-enters the translator) and leaves the outer
%accumulator exactly as it found it, error or not:
with_ctor_snapshot(F, Goal) :- ctor_deps(Saved),
                               nb_setval('$ctor_deps', []),
                               (  catch(Goal, E, ( nb_setval('$ctor_deps', Saved), throw(E) ))
                               -> ctor_deps(Ds),
                                  nb_setval('$ctor_deps', Saved),
                                  ( Ds == none -> true ; record_ctor_snapshots(F, Ds) )
                               ;  nb_setval('$ctor_deps', Saved), fail ).

%Same accumulator, for a verdict that is NOT a clause: returns the types read
%instead of attributing them to a function (the exhaustiveness prepass).
with_ctor_snapshot_types(Goal, Types) :- ctor_deps(Saved),
                                         nb_setval('$ctor_deps', []),
                                         (  catch(Goal, E, ( nb_setval('$ctor_deps', Saved), throw(E) ))
                                         -> ctor_deps(Ds),
                                            nb_setval('$ctor_deps', Saved),
                                            ( Ds == none -> Types = [] ; Types = Ds )
                                         ;  nb_setval('$ctor_deps', Saved), fail ).

record_ctor_snapshots(F, Ts) :- forall(member(T, Ts),
                                       ( ctor_key_snapshot(T, Keys),
                                         retractall(ctor_snapshot_use(T, _, F)),
                                         assertz(ctor_snapshot_use(T, Keys, F)) )).

%Everything domain_keys/3 and union_member_excluded/3 can see of T: its
%equation-less declared constructors and its declared nullary constants.
ctor_key_snapshot(T, Keys) :- findall(C/K, ( member_ctor(T, K, C)
                                           ; declared_value_type(C, T2), T2 == T, atom(C), \+ fun(C), K = 0 ),
                                      Ks),
                              sort(Ks, Keys).

%Hook on every NEW type declaration (maybe_cache_type_decl/2). Two gates keep
%it off the hot path, cheapest first: a program that has recorded no snapshot
%has nothing to revalidate (which is every program until it narrows a union or
%declares a -[det]->, and in particular the whole builtin-type seeding pass),
%and a declared symbol WITH equations is never a constructor anyway -
%member_ctor/3's own rule, since such a symbol is rewritten at the call site
%and never survives as a value.
note_constructor_set_change(_) :- \+ ctor_snapshot_use(_, _, _),
                                  \+ det_exhaustive_verdict(_, _, _, _, _, _, _, _), !.
note_constructor_set_change(Name) :- fun(Name), !.
note_constructor_set_change(Name) :- revalidate_ctor_snapshots(Name).

%A declaration only ever ADDS to a constructor set, and it adds exactly the
%keys built from the symbol being declared - so only THAT symbol has to be
%tested against each recorded snapshot. Recomputing whole key sets here
%instead would be quadratic in the number of declarations, for no extra
%precision.
new_ctor_key(Name, T, K) :- member_ctor(T, K, Name).
new_ctor_key(Name, T, 0) :- declared_value_type(Name, T2), T2 == T, \+ fun(Name).

revalidate_ctor_snapshots(Name) :-
    findall(F, ( ctor_snapshot_use(T, Keys, F),
                 new_ctor_key(Name, T, K), \+ memberchk(Name/K, Keys) ),
            Fs0),
    sort(Fs0, Fs),
    forall(member(F, Fs),
           ( format(user_error,
                    "Warning: a constructor declared after ~w was compiled changes a type it matched on; recompiling ~w~n",
                    [F, F]),
             recompile_function_clauses(F) )),
    revalidate_det_exhaustiveness(Name).

%Re-run every exhaustiveness verdict whose domain the new symbol enters. A
%verdict that now fails throws det_nonexhaustive/3 from here - reported
%against the clause that made the claim, not against the declaration that
%broke it, because the clause is what has to change (cover the new
%constructor, or say -[semidet]->).
%The location is the one the verdict was made at, in the file that made it -
%not the file being read now, which merely added the constructor:
revalidate_det_exhaustiveness(Name) :-
    forall(( det_exhaustive_verdict(F, N, Heads, Consts, Types, File, Line, Str),
             member(T, Types), once(new_ctor_key(Name, T, _)) ),
           in_metta_file(File, with_form_location(Line, Str, check_det_exhaustive(Consts, F, N, Heads)))).

in_metta_file(File, Goal) :- current_metta_file(Prev),
                             setup_call_cleanup(nb_setval('$metta_file', File),
                                                Goal,
                                                nb_setval('$metta_file', Prev)).

%An earlier branch consumed EVERY (Ctor V1 ... Vk) value: its pattern is
%headed by Ctor at that arity and its arguments are distinct variables, so the
%match cannot fail. A pattern like (CPU foo $a $r) constrains a field and
%consumes nothing.
prior_consumed_ctor(Prior, Ctor, K) :- member(P0, Prior), nonvar(P0), is_list(P0),
                                       P0 = [H|As], H == Ctor,
                                       length(As, K), maplist(var, As),
                                       sort(As, Distinct), length(Distinct, K), !.

%Check the clause body's inferred output type against the declared output type:
clause_output_goals(_, none, _, _, []) :- !.
clause_output_goals(F, out(OT, ATs), ExpOut, BodyExpr, Gs) :-
        %A declared output type variable occurring in no argument type claims
        %parametric polymorphism: only a bottom body (no value of its own) can
        %honor it, so a body with a concrete result type is rejected:
        ( var(OT) -> ( term_variables(ATs, Vs), \+ memberchk_eq(OT, Vs)
                       -> parametric_output_check(F, ExpOut) ; true ),
                     Gs = []
        ; wildcard_type_t(OT) -> Gs = []
        %quoted CODE is exempt from output checking; a quoted literal is just
        %that literal and is checked like any other value:
        ; nonvar(BodyExpr), BodyExpr = [Q, QV], Q == quote, \+ atomic(QV) -> Gs = []
        %EVERY branch feeding the result must fit the declared output type. A
        %branch of undetermined type (the marker) is not one that fits, so it
        %costs a runtime guard - or a rejection under --strict - exactly as an
        %entirely untyped body does. Without this, one typed branch discharged
        %the certification for all of them:
        %An INDEFINITE branch - the unknown marker, or a candidate still an
        %unbound declaration-instance type variable - is not one that fits
        %(see indefinite_candidate/1), so it costs a runtime guard, or a
        %rejection under --strict, exactly as an entirely untyped body does:
        ; var(ExpOut) ->
            ( known_candidates(ExpOut, Cs) ->
                ( member(C, Cs), \+ indefinite_candidate(C),
                  \+ type_compat_soft(C, OT), \+ refinement_pair(C, OT)
                  -> throw(error(type_conflict(existing(C), required(OT)), typecheck))
                ; member(C, Cs), ( indefinite_candidate(C) -> true ; \+ type_compat_soft(C, OT) )
                  -> type_guard(F, ExpOut, OT, Gs)            %possible runtime refinement
                   ; Gs = [] )
            ; type_guard(F, ExpOut, OT, Gs) )
        ; check_value(ExpOut, OT, St),
          ( St == mismatch -> throw(error(literal_type_mismatch(ExpOut, OT), typecheck))
          ; St == unknown -> type_guard(F, ExpOut, OT, Gs)
          ; Gs = [] ) ).

%Under --oracle a statically discharged output certification is re-verified
%at runtime with the checker's OWN value relation (check_value): a definite
%mismatch between the certified type and the actual value throws. The
%reflective guard is deliberately not used here - it is weaker than the
%checker (no constructor typing over erased brands, no quote exemption).
oracle_output_check(DeclOut, Out, Gs0, Gs) :-
    ( oracle_mode(true), DeclOut = out(OT, _), nonvar(OT), \+ wildcard_type_t(OT), Gs0 == []
      -> Gs = [oracle_check(Out, OT)]
       ; Gs = Gs0 ).

%The same treatment for the OTHER half of the certification story: an argument
%obligation that check_call_arg/5 discharged statically (the callee's declared
%parameter type was proved compatible, so no guard was emitted) is re-verified
%at runtime under --oracle. Output certifications alone left every call-site
%obligation unaudited, which is precisely where the constructor-snapshot and
%brand-erasure holes live. A pure addition: no compile-time decision consults
%these goals, they are appended after the branch has already been chosen.
%
%acyclic_term/1: an inferred self-referential type (examples/matespacefast.metta
%builds one) is a rational tree the compiler can hold but assertz/1 cannot store,
%so such a certification is simply not auditable this way and is skipped rather
%than crashing the compile. Reported here so it is not mistaken for a pass.
oracle_arg_check(AV, T, Gs) :- ( oracle_mode(true), nonvar(T), \+ wildcard_type_t(T),
                                 acyclic_term(T), acyclic_term(AV)
                                 -> Gs = [oracle_check(AV, T)]
                                  ; Gs = [] ).

%%% --oracle-det: the CARDINALITY oracle.
%
%A determinism declaration is a claim about how many solutions a call has:
%det = exactly one, semidet = zero or one. Nothing in the compiled program
%checks that claim, and the clause-entry ! actively hides violations by
%pruning the choicepoints that would reveal them. Under --oracle-det a call to
%a committed function is compiled to oracle_det_call/4, which enumerates the
%call's solutions, adjudicates the count, and then re-establishes the single
%solution's bindings. The callee is executed exactly ONCE (findall/3 drives it
%to exhaustion), so side effects are not duplicated - but last-call
%optimization is gone for wrapped calls, which is the price of counting.
oracle_det_wrap(Fun, NArgs, Out, Goal, Wrapped) :-
    ( oracle_det_mode(true), atom(Fun),
      oracle_det_believed(Fun, NArgs, Det), committed_det(Det)
      -> Wrapped = oracle_det_call(Fun, Det, Out, Goal)
       ; Wrapped = Goal ).

%What is audited is a PROMISE, so only a declared determinism selects a call
%for wrapping - inferred determinism (body_determinism/3) is nobody's promise,
%and the builtin table is the checker's own bookkeeping rather than a claim the
%program made. But where that table exists it OVERRIDES the declaration, just
%as function_call_determinism/3 does: (: empty (-> $a)) reads as det under
%--strict-det while the checker knows empty is the canonical semidet bottom,
%and auditing that call against the declaration would make the oracle stricter
%than the thing it audits - a false positive by construction.
oracle_det_believed(F, N, Det) :- catch(fn_determinism(F, N, Det0), _, fail),
                                  Det0 \== unspecified,
                                  ( builtin_call_determinism(F, N, DetB) -> Det = DetB ; Det = Det0 ).

%A call whose RESULT argument is already bound is not asking the function for
%its answer, it is testing a candidate one: (let True (> (myplus $x 2) 3) $x)
%compiles to a call to >/3 with the result fixed at true, and failing is how it
%says "no". "det" is a claim about the answering mode, so zero solutions is
%only a violation where the call site left the result open. Two or more
%solutions is a violation in either mode - no amount of input binding turns one
%answer into two.
oracle_det_call(F, Det, Out, Goal) :-
    ( var(Out) -> Answering = true ; Answering = false ),
    findall(Goal, Goal, Sols),
    length(Sols, N),
    ( N >= 2 -> throw(error(determinism_cardinality(F, Det, N), determinism))
    ; Sols == [] -> ( Det == det, Answering == true
                      -> throw(error(determinism_cardinality(F, Det, 0), determinism))
                       ; fail )                 %semidet may fail; a filter may too
    ; Sols = [S], Goal = S ).

%KNOWN LIMITATION: oracle_check/2 adjudicates with the checker's own
%check_value/3, so it audits the CERTIFICATIONS, not the type model. A value
%relation that is itself too permissive agrees with the certification it should
%contradict and stays invisible here - it can only ever re-ask the same
%question. That is why the (Newtype <wildcard>) hole had to be closed in
%type_unify/2 (see brand_unify/2) rather than instrumented: no oracle built on
%check_value/3 could have seen it. Auditing the model needs an INDEPENDENT
%value relation, which is out of scope.
%check_value/3 is a COMPILE-TIME relation over terms the compiler owns: it
%binds open types, and (via the partial(F,B) clause) it binds an unbound VALUE
%as well. Running it on live runtime data must not leak either binding back
%into the audited program - an oracle that mutates the program it audits is
%not an oracle. So both sides are copied, the call is made semidet, and an
%unbound value is treated as no evidence rather than as a certification.
oracle_check(V, T) :- ( var(V) -> true
                      ; copy_term(V-T, V2-T2),
                        ( once(check_value(V2, T2, St)), St == mismatch
                          -> throw(error(literal_type_mismatch(V, T), typecheck))
                           ; true ) ).

%The unknown marker is not a concrete result type, so it never turns a bottom
%body into a dishonest parametric declaration:
parametric_output_check(F, ExpOut) :- ( var(ExpOut)
                                        -> ( known_candidates(ExpOut, Cs), member(C, Cs),
                                             nonvar(C), \+ unknown_candidate(C)
                                             -> throw(error(non_parametric_output(F), typecheck)) ; true )
                                         ; throw(error(non_parametric_output(F), typecheck)) ).

%A declared arg type variable claims parametric universality over the position
%it occupies: callers passing any value are unchecked there. Snapshot EVERY
%type variable still unbound AFTER head-pattern binding (clause_param_types may
%already have instantiated some via head literals), including the ones buried
%inside a compound type.
%
%The nested ones used to be excluded, on the theory that "element typing may
%legitimately bind it". It does not: fn_decl_arity/4 hands every call site a
%FRESH COPY of the declaration, so a binding made while compiling the body
%touches only this clause's instance and nothing re-establishes it for callers.
%The binding therefore does not check anything - it silently ELIDES the check:
%
%    (: sumh (-> (List $a) Number))
%    (= (sumh (cons $h $t)) (+ $h 1))       % $a := Number, only here
%    !(sumh (cons "x" ()))                  % fresh $a := String, accepted
%
%compiled with zero runtime checks under --strict and printed 121 (SWI reads a
%one-character string as its character code). A body that pins a nested type
%variable is exactly as dishonest as one that pins a top-level one, and is
%rejected the same way: the declaration has to name the type the body needs.
parametric_param_snapshot(out(_, ATs), Vars) :- !, term_variables(ATs, Vars).
parametric_param_snapshot(_, []).

%%% The promised type variables of the clause currently being compiled.
%
% The snapshot above is a promise the declaration made to every caller, and
% two rules follow from that, both of which need to know the set while the
% BODY is being compiled (translate_clause/3 publishes it here):
%
%   1. Nothing the body reads may be discharged against one - it stands for a
%      type the caller picked, not one this clause knows (indefinite_candidate/1).
%   2. Nothing the compiler GUESSES may pin one. translate_closure_call/5
%      assumes an unknown head is a function and binds its type to an arrow
%      shape; that is sound inference about an undeclared function's own
%      parameter, but on a promised variable it is the compiler inventing a
%      fact about a position the declaration quantifies universally over -
%      and it costs nothing to skip, since a non-function head still reduces
%      to data exactly as before.
%
% Set with b_setval/2 so a nested compile (specialization, eval) that is later
% abandoned by backtracking cannot leak its set into the outer clause.
param_promises_scope(Promises, Outer) :- catch(b_getval('$param_promises', Outer), _, Outer = []),
                                         b_setval('$param_promises', Promises).

param_promises_restore(Outer) :- b_setval('$param_promises', Outer).

param_promise_var(V) :- var(V),
                        catch(b_getval('$param_promises', Vs), _, fail),
                        memberchk_eq(V, Vs).

%After the body is translated, every snapshotted position must still be unbound
%(var-var aliasing to another polymorphic function) or a wildcard. If the body
%forced it to a concrete type the declaration is dishonest - mirror
%parametric_output_check and reject at compile time:
parametric_param_check(F, Vars) :- forall(member(T, Vars),
                                          ( var(T) -> true
                                          ; wildcard_type_t(T) -> true
                                          ; throw(error(non_parametric_param(F, T), typecheck)) )).

%Strict mode: every compiled function needs a declared or inferred type
%(lambdas exempt). Checked after clause translation so inference can run first:
strict_check_function_typed(F, Args) :- ( strict_mode(true), \+ sub_atom(F, 0, _, _, 'lambda_')
                                          -> length(Args, N),
                                             ( fn_decl_arity(F, N, _, _) -> true
                                             ; inferred_decl_arity(F, N, _, _) -> true
                                             ; throw(error(strict_missing_function_type(F, N), typecheck)) )
                                           ; true ).

%%% Local type inference for undeclared functions %%%
%
% While an undeclared function's clause is translated, its variable parameters
% carry fresh assumption type variables; typed call sites in the body bind them
% by unification. A parameter whose assumption sees conflicting uses is tainted
% (no knowledge is recorded for it). This includes the variables of a
% destructuring head pattern - they are parameters of the clause too - whose
% assumptions are then rebuilt into the pattern's own type, the only thing a
% call site can go on once the body has stopped guarding them.
%
% The harvested types live in an internal store, are never asserted into &self,
% and are used only to *add* knowledge: eliminating guards, typing call
% outputs, and satisfying strict mode. Call sites of inferred functions never
% throw at compile time, and demand an inferred type only where the value is
% visibly of another one (see check_call_arg/5).
:- dynamic inferred_fn_type/3.     % inferred_fn_type(F, ArgTypes, OutType)

inferred_decl_arity(F, N, ATs, OT) :- inferred_fn_type(F, ATs, OT), length(ATs, N).

begin_clause_inference(F, Args, Assume, saved(OldA, OldD, OldT)) :-
        catch(b_getval('$assumptions', OldA), _, OldA = []),
        catch(b_getval('$assume_decl', OldD), _, OldD = none),
        catch(b_getval('$assump_taint', OldT), _, OldT = []),
        length(Args, N),
        ( \+ \+ fn_decl_arity(F, N, _, _) -> Assume = none, Pairs = [], Decl = none
                                           ; foldl(assume_param_type, Args, t([], []), t(PairsR, PTsR)),
                                             reverse(PairsR, Pairs), reverse(PTsR, PTs),
                                             Assume = assume(Pairs),
                                             Decl = d(F, N, PTs, _OutTv) ),
        b_setval('$assumptions', Pairs),
        b_setval('$assume_decl', Decl),
        b_setval('$assump_taint', []).

assume_param_type(Arg, t(Ps, Ts), t(Ps1, [T|Ts])) :- ( var(Arg)
                                                       -> ( known_singleton(Arg, T) -> Ps1 = Ps
                                                          ; add_known_type(Arg, T), Ps1 = [a(Arg, T)|Ps] )
                                                     ; value_single_type(Arg, T)
                                                       -> ctor_pattern_field_types(Arg), Ps1 = Ps
                                                     %a variable bound by a DESTRUCTURING head pattern is
                                                     %every bit as much a parameter of the clause, so it
                                                     %gets the same fresh assumption (the pattern's own
                                                     %type is rebuilt from those in infer_param_type/4):
                                                     ; is_list(Arg) -> assume_pattern_vars(Arg, Ps, Ps1)
                                                     ; Ps1 = Ps ).

assume_pattern_vars([], Ps, Ps).
assume_pattern_vars([A|As], Ps0, Ps) :- ( var(A) -> ( known_singleton(A, _) -> Ps1 = Ps0
                                                    ; add_known_type(A, Tv), Ps1 = [a(A, Tv)|Ps0] )
                                        ; is_list(A) -> assume_pattern_vars(A, Ps0, Ps1)
                                        ; Ps1 = Ps0 ),
                                        assume_pattern_vars(As, Ps1, Ps).

%A pattern headed by a uniquely declared constructor types its fields from that
%declaration, whatever the scrutinee's type turns out to be: (: P (-> Number
%Number Pair)) makes the $a and $b of (= (f (P $a $b)) ...) Numbers. Pure added
%knowledge - the fields of a value carrying P's tag are what P declared them.
%A literal field that contradicts the declaration only means this clause head
%cannot match a well-typed value; inference stays out of that judgement.
ctor_pattern_field_types(Arg) :- ( is_list(Arg), Arg = [Tag|Fs], atom(Tag), Fs \== [],
                                   length(Fs, N), findall(ATs, fn_decl_arity(Tag, N, ATs, _), [ATs1])
                                   -> catch(maplist(bind_param_type, Fs, ATs1),
                                            error(literal_type_mismatch(_, _), typecheck), true)
                                    ; true ).

taint_assumption(AV) :- ( catch(b_getval('$assumptions', Pairs), _, fail),
                          member(a(P, _), Pairs), P == AV
                          -> catch(b_getval('$assump_taint', Ts), _, Ts = []),
                             b_setval('$assump_taint', [AV|Ts])
                           ; true ).

end_clause_inference(F, Args, ExpOut, Assume, saved(OldA, OldD, OldT)) :-
        ( Assume = assume(Pairs) -> store_inferred_type(F, Pairs, Args, ExpOut) ; true ),
        b_setval('$assumptions', OldA),
        b_setval('$assume_decl', OldD),
        b_setval('$assump_taint', OldT).

%The provisional declaration of the clause being translated, for self-recursion:
assumed_self_decl(F, N, PTs, OutTv) :- catch(b_getval('$assume_decl', D), _, fail),
                                       D = d(F, N, PTs, OutTv).

store_inferred_type(F, Pairs, Args, ExpOut) :- catch(b_getval('$assump_taint', Taints), _, Taints = []),
                                               maplist(infer_param_type(Pairs, Taints), Args, ATs0),
                                               infer_out_type(ExpOut, OT0),
                                               maplist(normalize_inferred_param, ATs0, ATs1),
                                               maplist(pattern_type_roundtrip, Args, ATs1, ATs),
                                               normalize_inferred(OT0, OT),
                                               ( member(T, [OT|ATs]), T \== '%Undefined%'
                                                 -> merge_inferred(F, ATs, OT) ; true ).

%A structural parameter type is only worth storing if it still ACCEPTS the very
%pattern it was read off. The two readings of a tuple type (see
%tagged_tuple_type/3) do not compose freely: (Statement $s $p) under a declared
%(: Statement Type) infers (Type Number Number), which reads back as a tagged
%shape demanding the literal atom Type in head position - a claim no value
%matching that pattern satisfies. Round-tripping the pattern rejects exactly
%those, whichever way the type was built.
pattern_type_roundtrip(Arg, T, TN) :- ( \+ is_list(Arg) -> TN = T
                                      ; T \== '%Undefined%', \+ \+ check_value(Arg, T, ok) -> TN = T
                                      ; TN = '%Undefined%' ).

%The type of a destructuring parameter is its pattern with each field replaced
%by the field's inferred type - (stv $s $c) with both fields used as numbers is
%(stv Number Number). Rebuilding it is not decoration: the fields carry
%assumption types now, so the body no longer guards them, and this is what
%keeps a call site checking that (stv "a" "b") is not one of those.
infer_param_type(Pairs, Taints, Arg, T) :- ( var(Arg) -> ( memberchk_eq(Arg, Taints) -> T = '%Undefined%'
                                                         ; member(a(P, Tv), Pairs), P == Arg -> T = Tv
                                                         ; known_singleton(Arg, K) -> T = K
                                                         ; T = '%Undefined%' )
                                           ; value_single_type(Arg, T0) -> T = T0
                                           ; is_list(Arg), Arg = [Tag|Fs], atom(Tag), Fs \== [],
                                             maplist(infer_param_type(Pairs, Taints), Fs, FTs)
                                             -> T = [Tag|FTs]
                                           ; T = '%Undefined%' ).

infer_out_type(Out, T) :- ( var(Out) -> ( known_singleton(Out, K) -> T = K ; T = '%Undefined%' )
                          ; value_single_type(Out, T0) -> T = T0
                          ; T = '%Undefined%' ).

%Only clearly usable shapes are recorded; everything else is no-knowledge:
normalize_inferred(T, '%Undefined%') :- var(T), !.
normalize_inferred(T, T) :- atom(T), !.
normalize_inferred(T, ['List', ETN]) :- ground(T), list_type(T, ET), !,
                                        normalize_inferred(ET, ETN).
normalize_inferred(T, T) :- ground(T), is_arrow_type(T), !.
normalize_inferred(_, '%Undefined%').

%A destructuring parameter's shape is knowledge too, so it survives instead of
%collapsing - but only if it will be READ BACK as the tagged shape it was built
%from. (Statement $s $p) under a declared (: Statement Type) rebuilds to
%(Statement T1 T2), which tagged_tuple_type/3 reads positionally, as a 3-field
%record whose first field is a Statement: a different, and false, claim about
%the value. An undefined field collapses the whole shape as well - a partly
%known tuple type is not a shape any call site can check. Parameters only:
%their shapes are rebuilt from patterns this file controls and are verified
%against those patterns afterwards (pattern_type_roundtrip/3), which is not
%true of an output type read off an arbitrary body expression.
normalize_inferred_param(T, TN) :- ( is_list(T), T = [Tag|Fs], atom(Tag), Fs \== [],
                                     maplist(normalize_inferred_param, Fs, FTs),
                                     \+ memberchk('%Undefined%', FTs),
                                     TN0 = [Tag|FTs], tagged_tuple_type(TN0, Tag, FTs)
                                     -> TN = TN0
                                      ; normalize_inferred(T, TN) ).

%Clauses of the same function are joined position-wise; disagreement widens:
merge_inferred(F, ATs, OT) :- length(ATs, N),
                              ( inferred_decl_arity(F, N, ATs0, OT0)
                                -> retract(inferred_fn_type(F, ATs0, OT0)),
                                   maplist(join_inferred, ATs0, ATs, ATs1),
                                   join_inferred(OT0, OT, OT1),
                                   ( member(T, [OT1|ATs1]), T \== '%Undefined%'
                                     -> assertz(inferred_fn_type(F, ATs1, OT1)) ; true )
                                 ; assertz(inferred_fn_type(F, ATs, OT)) ).

join_inferred(A, B, J) :- ( A =@= B -> J = A ; J = '%Undefined%' ).

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
    ( committed_det(Det) -> ensure_deterministic_expr(Det, BodyExpr, F),
                            ensure_non_overlapping_clause_heads(F, Args, PrevClauses)
                          ; true ).

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

%%% Determinism of the builtins (registered funs backed by a Prolog predicate
%%% rather than MeTTa equations).
%
% This table is the ONLY determinism knowledge the checker has about them: a
% builtin that is not listed is `unspecified`, which is what "never analysed"
% honestly means. It used to be the opposite - any registered symbol whose
% Prolog predicate merely existed was assumed det - and that assumed the
% strongest possible claim about the one part of the system the analysis
% cannot see. (get-atoms) is the counterexample that motivated the flip: it
% enumerates a space's clauses, so a -[det]-> function bound to it produced
% one result per atom in the space.
%
% THE CALLING CONVENTION THAT ACTUALLY HOLDS.
%
% Arity N is the MeTTa argument count; the Prolog predicate has N+1 arguments,
% the last being the result. Beyond that, assume NOTHING:
%
%   * An argument position may be UNBOUND, and may hold a value of the wrong
%     type. No check the compiler emits rules either out. The residual guard is
%     typecheck_or_error/2, whose variable branch is constrain_var_type/2, and
%     that SUCCEEDS on an unbound variable - it records a requirement for later,
%     it does not test one. So "typed Bool" never implies "bound to a boolean",
%     and "typed (List T)" never implies "a proper list". An unbound argument of
%     a declared type arises from ordinary well-typed code: the constructor
%     application (B $u) leaves its Bool field unfilled.
%   * The result position is usually a fresh variable, but not always - a call
%     whose result is already bound is testing a candidate answer rather than
%     asking for one (see oracle_det_call/4).
%   * An exception is not a solution. A predicate that raises on a mode it
%     cannot serve is still det; one that FAILS on such a mode is at best
%     semidet, and one that enumerates is nondet.
%
% So: det = exactly one solution for every instantiation, semidet = at most
% one, nondet = no claim beyond "some". Where none of the three can be
% established, the entry is simply absent, and an unlisted builtin is
% `unspecified` - which is what "never analysed" honestly means.
%
% The previous preamble assumed a ground, well-typed convention, and in
% particular that a (List T) position carries a PROPER list. It does not, and
% that assumption is what made eighteen entries false.
%
% The remedy applied throughout is to correct the ENTRY, not the predicate. In
% every one of these cases the extra solutions are real: nth0/3 enumerates
% actual elements, append/3 inverts to solve for a prefix, length/2 enumerates
% the shapes an open list can take, and bool/1 enumerates a finite type. The
% relational modes are used on purpose - lib_roman's mylast/init/rcons invert
% union-atom, examples/logicprogset.metta solves for a list from its length,
% and examples/booleansolver.metta enumerates boolean assignments - so a
% predicate answering more than once is behaving correctly and the table was
% simply wrong about it. A determinism table is a description; changing what a
% builtin DOES so that a stale description becomes true would be a language
% change smuggled in as a bug fix.
%
% The cost is that a -[det]-> body may no longer call them, which is exactly
% the point: it could not have kept its promise while doing so.
%
% Every entry below was re-derived from the predicate's source (src/metta.pl,
% src/spaces.pl, src/parser.pl, or the SWI library) and checked by counting
% solutions with every argument unbound. Do not trust these comments over the
% source; the previous set of comments is what got this wrong.

%--- Nondeterministic: more than one solution by construction.
builtin_call_determinism(superpose, 1, nondet).
%(get-atoms Space) backtracks over current_predicate/1 and clause/2 -
%one solution per atom in the space (src/spaces.pl):
builtin_call_determinism('get-atoms', 1, nondet).
%match/4 backtracks over the space relation the same way (src/spaces.pl):
builtin_call_determinism(match, 3, nondet).
%'get-type'/2 collects candidates through a SOFT cut (*->), so every
%get_type_candidate/2 solution is offered - and it is dynamic, so user
%refinement clauses add more (src/metta.pl):
builtin_call_determinism('get-type', 1, nondet).
%member(X, L, true) :- member(X, L) - one solution per matching element,
%and 'is-member' has that same generator in its first clause (src/metta.pl):
builtin_call_determinism(member, 2, nondet).
builtin_call_determinism('is-member', 2, nondet).
%callPredicate calls an arbitrary Prolog goal (src/metta.pl):
builtin_call_determinism(callPredicate, 1, nondet).
%bool/1 is two facts, so it ENUMERATES a boolean it was not given: with an
%unbound argument each of these answers twice. Their (-> Bool Bool Bool)
%declaration guarantees nothing about boundness - see the convention above -
%and an unbound Bool arises from ordinary well-typed code, e.g. the field of
%a constructor application (B $u). The enumeration is used deliberately
%(examples/booleansolver.metta solves (and (or $x True) $y) for $x and $y), so
%the entries are what was wrong (was: det, src/metta.pl):
builtin_call_determinism(and, 2, nondet).
builtin_call_determinism(or, 2, nondet).
builtin_call_determinism(not, 1, nondet).
builtin_call_determinism(xor, 2, nondet).
builtin_call_determinism(implies, 2, nondet).
%index-atom's guard rejects only an index BOUND to a non-integer; an unbound
%one falls through to nth0/3, which enumerates the list - three solutions from
%(index-atom (1 2 3) $i) (was: semidet, src/metta.pl):
builtin_call_determinism('index-atom', 2, nondet).
%The list operations, PeTTa's own wrappers and the library predicates it
%exposes by name alike. Each recurses on, or measures, a list argument, and
%each INVERTS when that argument is partial or unbound: append/3 over every
%split, length/2 over every length (so (size-atom $u) does not terminate),
%reverse/2 and last/2 likewise, and select/3 inside subtraction-atom and
%intersection-atom. Their non_list/1 guards do not catch it - non_list/1 is
%true only of a term that can never BECOME a list.
%
%Nothing in the compiled program establishes a proper list: (: size-atom
%(-> $a Number)) is a bare type variable, so check_call_arg/5 emits nothing at
%all, and even a (List T) declaration only gets typecheck_or_error/2, which
%succeeds on an unbound variable. The inverse modes are the point of
%lib_roman's mylast/init/rcons and of examples/logicprogset.metta, so again it
%is the entries that were wrong (was: det, except last/1 semidet):
builtin_call_determinism('size-atom', 1, nondet).
builtin_call_determinism('union-atom', 2, nondet).
builtin_call_determinism('subtraction-atom', 2, nondet).
builtin_call_determinism('intersection-atom', 2, nondet).
builtin_call_determinism('exclude-item', 2, nondet).
builtin_call_determinism('alpha-unique-atom', 1, nondet).
builtin_call_determinism(append, 2, nondet).
builtin_call_determinism(reverse, 1, nondet).
builtin_call_determinism(length, 1, nondet).
builtin_call_determinism(last, 1, nondet).

%--- Semidet: at most one solution, but the input may not match.
%(empty) produces zero results, never two: it is the canonical semidet body,
%and the reason a -[semidet]-> function can write its fallthrough explicitly:
builtin_call_determinism(empty, 0, semidet).
%Single non-total clauses: they fail on a value of the wrong shape
%(first/2 and the pair selectors want a 2-element list, decons a non-empty
%one), and nth0/3 fails on an out-of-range index (src/metta.pl):
builtin_call_determinism(first, 1, semidet).
builtin_call_determinism('first-from-pair', 1, semidet).
builtin_call_determinism('second-from-pair', 1, semidet).
builtin_call_determinism(decons, 1, semidet).
builtin_call_determinism('decons-atom', 1, semidet).
%min_list/2 and max_list/2 fail on the empty list, and raise on a partial or
%unbound one - neither is a second solution (src/metta.pl):
builtin_call_determinism('min-atom', 1, semidet).
builtin_call_determinism('max-atom', 1, semidet).
%nb_getval/2 RAISES on an unset key (existence_error), it does not fail - the
%old comment had this backwards. That would make get-state det, but semidet is
%the weaker claim and nothing needs the stronger one, so it stays (src/metta.pl):
builtin_call_determinism('get-state', 1, semidet).
%random_between/3 FAILS when Max < Min - it does not raise and it does not
%clamp - so (random-int 5 1) has ZERO solutions and a -[det]-> body calling it
%silently produces nothing (was: det, src/metta.pl):
builtin_call_determinism('random-int', 2, semidet).
%bind!'s only clause is 'bind!'(A, ['new-state', B], C): the second argument
%must literally be a (new-state ...) expression, so the standard idiom
%(bind! &x V) matches nothing and fails (was: det, src/metta.pl):
builtin_call_determinism('bind!', 2, semidet).
%get-metatype's eight clauses - variable, number, string, the two booleans, a
%registered fun, a list, an atom - do not cover every value: a partial
%application is the compound partial(F, Bound), which is neither a list nor an
%atom, so the call fails (was: det, src/metta.pl):
builtin_call_determinism('get-metatype', 1, semidet).
%add_sexp/remove_sexp take an expression, [Rel|Args]. A bare symbol matches
%neither that nor the (= ...) function clause, so (add-atom &self foo) fails
%(was: det, src/spaces.pl):
builtin_call_determinism('add-atom', 2, semidet).
builtin_call_determinism('remove-atom', 2, semidet).

%--- Det: exactly one solution, no choicepoint left.
%Arithmetic and the math builtins are a single `Out is <expr>` clause. is/2
%raises on a non-number AND on an unbound argument, and an exception is
%neither a second solution nor a missing one (src/metta.pl):
builtin_call_determinism('+', 2, det).
builtin_call_determinism('-', 2, det).
builtin_call_determinism('*', 2, det).
builtin_call_determinism('/', 2, det).
builtin_call_determinism('%', 2, det).
builtin_call_determinism(min, 2, det).
builtin_call_determinism(max, 2, det).
builtin_call_determinism(exp, 1, det).
builtin_call_determinism('pow-math', 2, det).
builtin_call_determinism('log-math', 2, det).
builtin_call_determinism('sqrt-math', 1, det).
builtin_call_determinism('abs-math', 1, det).
builtin_call_determinism('trunc-math', 1, det).
builtin_call_determinism('ceil-math', 1, det).
builtin_call_determinism('floor-math', 1, det).
builtin_call_determinism('round-math', 1, det).
builtin_call_determinism('sin-math', 1, det).
builtin_call_determinism('cos-math', 1, det).
builtin_call_determinism('tan-math', 1, det).
builtin_call_determinism('asin-math', 1, det).
builtin_call_determinism('acos-math', 1, det).
builtin_call_determinism('atan-math', 1, det).
builtin_call_determinism('isnan-math', 1, det).
builtin_call_determinism('isinf-math', 1, det).
%library(quintus) arithmetic wrappers, also a single `Y is f(X)`:
builtin_call_determinism(sqrt, 1, det).
builtin_call_determinism(log, 1, det).
builtin_call_determinism(sin, 1, det).
builtin_call_determinism(cos, 1, det).
%Comparisons are one clause whose body is an if-then-else, so exactly one of
%true/false comes back. The arithmetic ones raise on an unbound argument;
%==, !=, =, =?, =alpha and =@= are total over any term (src/metta.pl):
builtin_call_determinism('<', 2, det).
builtin_call_determinism('>', 2, det).
builtin_call_determinism('<=', 2, det).
builtin_call_determinism('>=', 2, det).
builtin_call_determinism('==', 2, det).
builtin_call_determinism('!=', 2, det).
builtin_call_determinism('=', 2, det).
builtin_call_determinism('=?', 2, det).
builtin_call_determinism('=alpha', 2, det).
builtin_call_determinism('=@=', 2, det).
%clpfd wrappers: posting a constraint succeeds once; the reified comparisons
%are cut-then-fallback pairs, so exactly one of true/false (src/metta.pl):
builtin_call_determinism('#+', 2, det).
builtin_call_determinism('#-', 2, det).
builtin_call_determinism('#*', 2, det).
builtin_call_determinism('#div', 2, det).
builtin_call_determinism('#//', 2, det).
builtin_call_determinism('#mod', 2, det).
builtin_call_determinism('#min', 2, det).
builtin_call_determinism('#max', 2, det).
builtin_call_determinism('#<', 2, det).
builtin_call_determinism('#>', 2, det).
builtin_call_determinism('#=', 2, det).
builtin_call_determinism('#\\=', 2, det).
%Reflection: one clause each, if-then-else over a mode test, total over any
%term (src/metta.pl). get-metatype is NOT here - it is semidet, see above:
builtin_call_determinism('is-var', 1, det).
builtin_call_determinism('is-ground', 1, det).
builtin_call_determinism('is-expr', 1, det).
builtin_call_determinism('is-space', 1, det).
%Identity and rendering (src/metta.pl, src/parser.pl):
builtin_call_determinism(id, 1, det).
builtin_call_determinism(repr, 1, det).
builtin_call_determinism(repra, 1, det).
%Cell operations: a single always-matching clause, or a cut-guarded pair. An
%unbound argument is BOUND by the head pattern here, which is one solution
%rather than several (src/metta.pl):
builtin_call_determinism(cons, 2, det).
builtin_call_determinism('cons-atom', 2, det).
builtin_call_determinism('car-atom', 1, det).
builtin_call_determinism('cdr-atom', 1, det).
%is-alpha-member commits with a cut and falls back to false (src/metta.pl):
builtin_call_determinism('is-alpha-member', 2, det).
%The two list wrappers that ARE det, and for a reason nothing else in the
%family shares: the library call under their non_list/1 guard RAISES on a
%partial or unbound list (msort/2 and list_to_set/2 both demand a proper one)
%instead of enumerating, and an exception is not an extra solution
%(src/metta.pl). Contrast size-atom and union-atom above:
builtin_call_determinism('sort-atom', 1, det).
builtin_call_determinism('unique-atom', 1, det).
%The same reason, for the library predicates PeTTa exposes directly - unlike
%append/reverse/last/length above, these raise rather than enumerate:
builtin_call_determinism(sort, 1, det).
builtin_call_determinism(msort, 1, det).
builtin_call_determinism(list_to_set, 1, det).
builtin_call_determinism(atom_chars, 1, det).
%Total over any term:
builtin_call_determinism(copy_term, 1, det).
builtin_call_determinism(term_hash, 1, det).
%Effects and state: one solution, the effect is not a choicepoint
%(src/metta.pl). add-atom, remove-atom and bind! are NOT here - they can fail,
%see the semidet block above:
builtin_call_determinism('change-state!', 2, det).
builtin_call_determinism('println!', 1, det).
builtin_call_determinism('readln!', 0, det).
builtin_call_determinism(test, 2, det).
builtin_call_determinism(assert, 1, det).
builtin_call_determinism('current-time', 0, det).
builtin_call_determinism('format-time', 1, det).
builtin_call_determinism('add-translator-rule!', 1, det).
builtin_call_determinism('remove-translator-rule!', 1, det).
builtin_call_determinism(import_prolog_function, 1, det).
builtin_call_determinism('Predicate', 1, det).
builtin_call_determinism(assertaPredicate, 1, det).
builtin_call_determinism(assertzPredicate, 1, det).
builtin_call_determinism(retractPredicate, 1, det).
%Min + R * (Max - Min) is defined for every pair of numbers, Max < Min
%included, and raises on an unbound one - unlike random-int, which fails
%(src/metta.pl):
builtin_call_determinism('random-float', 2, det).
%
%Deliberately NOT listed, and therefore unspecified:
%  atom_concat/2, concat/2 - atom_concat/3 with both inputs unbound is a
%    generator over every split of the result, and no declared type rules
%    that mode out (nothing does - see the convention at the top).
%  foldl/3, maplist/2..4, 'foldl-atom', 'map-atom', 'filter-atom' - their
%    determinism is that of the closure they are given, which this table
%    cannot express.
%  'py-call', 'import!', eval, reduce, argv, library, exists_file,
%    'get-mettatype', 'mm2-exec', set_hook - foreign, dynamic, or absent
%    predicates whose result count is not established here.

function_call_determinism(F, N, Det) :- builtin_call_determinism(F, N, Det), !.
function_call_determinism(F, N, Det) :- catch(fn_determinism(F, N, Det0), _, fail),
                                        Det0 \== unspecified, !, Det = Det0.
function_call_determinism(F, N, Det) :- body_determinism(F, N, Det).

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
                                    clause_set_determinism(Metas1, Det0),
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
clause_bodies_determinism([fun_meta(_, B)|Ms], R) :- deterministic_expr(B, R1),
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
         ; \+ is_arrow_type(K) -> combine_determinism_list(Args, Result)  %non-arrow head: data construction
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
%A two-argument if has no else branch: when the condition is false the whole
%expression produces NOTHING. That is a failure path of the construct itself,
%invisible to an analysis of the parts, so it is may_fail unconditionally -
%-[semidet]-> accepts it, -[det]-> does not:
deterministic_expr([if, Cond, Then], Result) :- !, combine_determinism_list([Cond, Then], R0),
                                                combine_det_results(may_fail(if_without_else), R0, Result).
deterministic_expr([if, Cond, Then, Else], Result) :- !, combine_determinism_list([Cond, Then, Else], Result).
deterministic_expr([progn|Exprs], Result) :- !, combine_determinism_list(Exprs, Result).
deterministic_expr([prog1|Exprs], Result) :- !, combine_determinism_list(Exprs, Result).
deterministic_expr([let, Pat, Val, In], Result) :- !, pattern_then_exprs(Pat, [Val, In], Result).
deterministic_expr([chain, Pat, Val, In], Result) :- !, pattern_then_exprs(Pat, [Val, In], Result).
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
                                               function_call_determinism(Fun, N, Det),
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
                                        clause_set_determinism(Upgraded, Det0),
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

binds_and_body_determinism([], Body, Result) :- deterministic_expr(Body, Result).
binds_and_body_determinism([[Pat, Val]|Rest], Body, Result) :-
    pattern_then_exprs(Pat, [Val], HeadResult),
    ( det_result_final(HeadResult) -> Result = HeadResult
    ; binds_and_body_determinism(Rest, Body, R2),
      combine_det_results(HeadResult, R2, Result) ).

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

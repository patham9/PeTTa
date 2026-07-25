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

%Soundness oracles (see examples/soundness_matrix.sh): --oracle re-emits the
%output guard even where the checker discharged it statically, so a wrong
%certification fails at runtime; --no-det-cut suppresses the determinism
%commit, so a semantically nondeterministic det function shows extra results.
:- dynamic oracle_mode/1.
:- dynamic suppress_det_cut/1.
:- current_prolog_flag(argv, Argv),
   ( memberchk('--oracle', Argv) -> assertz(oracle_mode(true)) ; assertz(oracle_mode(false)) ),
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
                                                ; warn_if_late_declaration(Name),
                                                  assertz(declared_fn_type(Name, ATN, OTN, Det)) )
                                              ; normalize_type(Type, TN),
                                                ( declared_value_type(Name, T2), T2 =@= TN -> true
                                                ; assertz(declared_value_type(Name, TN)) ) )
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

%Type declarations only affect later forms; warn when one arrives after the
%function's clauses were already compiled (a silent no-op otherwise):
warn_if_late_declaration(Name) :-
    ( catch(nb_getval(Name, [_|_]), _, fail)
      -> format(user_error,
                "Warning: type declaration for ~w arrives after its definition; already-compiled clauses and earlier calls are unaffected~n",
                [Name])
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
type_unify(A, B) :- ( wildcard_type_t(A) ; wildcard_type_t(B) ), !.
%Union types (| T1 T2 ...): a union value must fit every context member-wise;
%a value fits a required union if it fits some member:
type_unify(A, B) :- is_union(A), !, A = ['|'|As],
                    \+ ( member(MA, As), \+ type_compat_soft(MA, B) ).
type_unify(A, B) :- is_union(B), !, B = ['|'|Ms],
                    member(M, Ms), type_unify(A, M), !.
%Newtypes are nominal: identical brands unify, a brand fits its
%representation, but neither the bare representation nor a different brand
%fits it implicitly (that is the point):
type_unify(A, B) :- atom(A), declared_newtype(A, RA), !,
                    ( atom(B) -> ( declared_newtype(B, _) -> A == B ; type_unify(RA, B) )
                               ; type_unify(RA, B) ).
type_unify(A, B) :- atom(B), declared_newtype(B, _), !, atom(A), A == B.
type_unify(A, B) :- atom(A), !, A == B.
%Arrows: a det closure fits anywhere, a nondet closure only fits a nondet
%requirement once --strict-det makes plain -> a determinism commitment:
type_unify(A, B) :- is_arrow_type(A), is_arrow_type(B), !,
                    A = [HA|As], B = [HB|Bs],
                    det_arrow_fits(HA, HB),
                    same_length(As, Bs), maplist(type_unify, As, Bs).
type_unify(A, B) :- is_list(A), !, is_list(B), same_length(A, B), maplist(type_unify, A, B).
type_unify(A, B) :- A == B.

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
add_known_type(V, T) :- ( get_attr(V, tknown, Cs) -> ( Cs = [K], var(K) -> K = T
                                                      ; variant_member(T, Cs) -> true
                                                      ; put_attr(V, tknown, [T|Cs]) )
                                                   ; put_attr(V, tknown, [T]) ).

known_candidates(V, Cs) :- get_attr(V, tknown, Cs).
known_singleton(V, K) :- get_attr(V, tknown, [K]).

%Propagate Val's statically known type(s) into Out (branch and binding flows):
note_candidates(Out, Val) :- ( var(Out)
                               -> ( nonvar(Val) -> ( value_single_type(Val, VT)
                                                     -> add_known_type(Out, VT) ; true )
                                  ; known_candidates(Val, Cs) -> add_known_types(Out, Cs)
                                  ; true )
                                ; true ).

%Explicit type ascription (the Type Expr): the author states the type of a
%dynamically typed value. The type becomes knowledge for the checker, and a
%runtime check is emitted even under --strict: strict mode forbids *implicit*
%residual checks, while an ascription is an explicit, visible boundary.
%An ascription that contradicts static knowledge is a compile-time error.
ascribe_type(V, T, Gs) :- ( var(T) -> Gs = []
                          ; wildcard_type_t(T) -> Gs = []
                          ; var(V) ->
                              ( known_singleton(V, K), var(K)
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
                                                  det_arrow_head(Det, H),
                                                  append(ATs, [OT], Xs) ), Fs),
                                append(Vs, Fs, Cs0),
                                ( Cs0 == [], current_arithmetic_function(V)
                                  -> Cs = ['Number']                %arithmetic constants: inf, nan, pi, e
                                   ; Cs = Cs0 ).
value_candidate_types(partial(F, B), Cs) :- !,
                                length(B, N),
                                findall([H|Xs], ( fn_decl_partial(F, N, PTs, RTs, OT, Det),
                                                  det_arrow_head(Det, H),
                                                  bound_args_match(B, PTs),
                                                  append(RTs, [OT], Xs) ), Cs).
value_candidate_types([], [['List', _]]) :- !.
%A constructor application (STV 0.5 0.8) has the constructor's output type,
%but only when its fields do not contradict the constructor's signature -
%otherwise the value is unknown and the (runtime or strict) guard decides:
value_candidate_types([H|Args], Cs) :- atom(H), length(Args, N), fn_decl_arity(H, N, _, _), !,
                                findall(OT, ( fn_decl_arity(H, N, ATs, OT),
                                              bound_args_match(Args, ATs) ), Cs).
value_candidate_types(V, Cs) :- is_list(V), maplist(value_single_type, V, Ts), !, Cs = [Ts].
value_candidate_types(_, []).

value_single_type(V, T) :- ( var(V) -> known_singleton(V, T)
                                     ; value_candidate_types(V, [T0]), T = T0 ).

det_arrow_head(Det, H) :- nonvar(Det), arrow_atom_det(H, Det), !.
det_arrow_head(_, (->)).

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

%%% Effectful call-site argument checking, one arg. Mode 'declared' throws on
%%% literal mismatches; mode 'inferred' only ever adds knowledge and guards:
check_call_arg(Mode, Fun, AV, T, Gs) :- ( var(AV)
                                          -> ( known_singleton(AV, K)
                                               -> ( nonvar(T), wildcard_type_t(T) -> Gs = []  %wildcards carry no knowledge
                                                  ; type_unify(K, T) -> Gs = []
                                                  %conflicting brands cannot be deferred to a runtime
                                                  %guard - newtypes are erased there - so they reject now:
                                                  ; atom(T), declared_newtype(T, _), atom(K), declared_newtype(K, _)
                                                    -> throw(error(type_conflict(existing(K), required(T)), typecheck))
                                                  ; taint_assumption(AV),  %known conflict: runtime error carries the value
                                                    type_guard(Fun, AV, T, Gs) )
                                             ; var(T) -> Gs = []
                                             ; wildcard_type_t(T) -> Gs = []
                                             ; type_guard(Fun, AV, T, Gs) )
                                        ; check_value(AV, T, St),
                                          ( St == ok -> Gs = []
                                          ; St == mismatch
                                            -> ( Mode == declared
                                                 -> throw(error(literal_type_mismatch(AV, T), typecheck))
                                                  ; type_guard(Fun, AV, T, Gs) )
                                          ; type_guard(Fun, AV, T, Gs) ) ).

%Open structured types (e.g. (List $a)) still guard their outer shape; only a
%fully unconstrained type variable needs no check at all:
type_guard(Fun, AV, T, Gs) :- ( nonvar(T), \+ wildcard_type_t(T)
                                -> ( strict_mode(true)
                                     -> throw(error(strict_runtime_typecheck(Fun, typecheck_or_error(AV, T)), typecheck))
                                      ; warn_residual_check(Fun, T),
                                        guard_goal(AV, T, G), Gs = [G] )
                                 ; Gs = [] ).

%Inline the primitive fast path into the compiled goal so hot code only pays a
%native type test; the reflective check runs only when that test fails:
guard_goal(AV, 'Number', ( number(AV) -> true ; typecheck_or_error(AV, 'Number') )) :- !.
guard_goal(AV, 'String', ( string(AV) -> true ; typecheck_or_error(AV, 'String') )) :- !.
guard_goal(AV, 'Bool', ( ( AV == true ; AV == false ) -> true ; typecheck_or_error(AV, 'Bool') )) :- !.
guard_goal(AV, T, typecheck_or_error(AV, T)).

apply_call_args(Mode, Fun, AVs, ATs, Gs) :- ( same_call_var_conflict(AVs, ATs) -> Gs = [fail]
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
%known type, e.g. (let (Stats $sum $sq $n) (make-stats) ...):
bind_pattern_from(Pat, Val) :- ( nonvar(Pat),
                                 ( var(Val) -> known_singleton(Val, KT)
                                             ; value_single_type(Val, KT) )
                                 -> bind_pattern_typed(Pat, KT)
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
%LIMITATION: (a) reads the constructor set as it stands right now. A
%constructor for Member declared in a LATER file (or a later form) would
%invalidate an exclusion already made; unlike get-type extensions there is no
%single hook to gate on, and the already-compiled clause is not revisited.
%Declare a type's constructors before the code that matches on it.
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
        ; var(ExpOut) ->
            ( known_candidates(ExpOut, Cs) ->
                ( member(C, Cs), \+ type_compat_soft(C, OT), \+ refinement_pair(C, OT)
                  -> throw(error(type_conflict(existing(C), required(OT)), typecheck))
                ; member(C, Cs), \+ type_compat_soft(C, OT)
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

oracle_check(V, T) :- copy_term(T, T2), check_value(V, T2, St),
                      ( St == mismatch
                        -> throw(error(literal_type_mismatch(V, T), typecheck)) ; true ).

parametric_output_check(F, ExpOut) :- ( var(ExpOut)
                                        -> ( known_candidates(ExpOut, Cs), member(C, Cs), nonvar(C)
                                             -> throw(error(non_parametric_output(F), typecheck)) ; true )
                                         ; throw(error(non_parametric_output(F), typecheck)) ).

%A declared arg type that is a bare type variable claims parametric universality
%over that position: callers passing any value are unchecked. Snapshot the
%positions that are still entirely var AFTER head-pattern binding (clause_param_types
%may already have instantiated some via head literals); a var buried inside a
%compound type like (List $a) is NOT recorded, since element typing may legitimately
%bind it:
parametric_param_snapshot(out(_, ATs), Vars) :- !, include(var, ATs, Vars).
parametric_param_snapshot(_, []).

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
% (no knowledge is recorded for it). The harvested types live in an internal
% store, are never asserted into &self, and are used only to *add* knowledge:
% eliminating guards, typing call outputs, and satisfying strict mode. Call
% sites of inferred functions never throw at compile time.
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
                                                     ; value_single_type(Arg, T) -> Ps1 = Ps
                                                     ; Ps1 = Ps ).

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
                                               maplist(normalize_inferred, ATs0, ATs),
                                               normalize_inferred(OT0, OT),
                                               ( member(T, [OT|ATs]), T \== '%Undefined%'
                                                 -> merge_inferred(F, ATs, OT) ; true ).

infer_param_type(Pairs, Taints, Arg, T) :- ( var(Arg) -> ( memberchk_eq(Arg, Taints) -> T = '%Undefined%'
                                                         ; member(a(P, Tv), Pairs), P == Arg -> T = Tv
                                                         ; known_singleton(Arg, K) -> T = K
                                                         ; T = '%Undefined%' )
                                           ; value_single_type(Arg, T0) -> T = T0
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

builtin_call_determinism(superpose, 1, nondet).
%(empty) produces zero results, never two: it is the canonical semidet body,
%and the reason a -[semidet]-> function can write its fallthrough explicitly:
builtin_call_determinism(empty, 0, semidet).

function_call_determinism(F, N, Det) :- builtin_call_determinism(F, N, Det), !.
function_call_determinism(F, N, Det) :- catch(fn_determinism(F, N, Det0), _, fail),
                                        Det0 \== unspecified, !, Det = Det0.
function_call_determinism(F, N, Det) :- body_determinism(F, N, Det).

%A deterministic caller needs positive evidence about its callees. Functions
%without a determinism arrow are analyzed from their translated clauses
%(bodies deterministic, heads non-overlapping), memoized, and treated as det
%on cycles (a recursive call cannot introduce what the rest disproves).
%A registered predicate with no MeTTa clauses is a Prolog builtin: det
%unless listed in builtin_call_determinism.
:- dynamic det_analysis_cache/3.

body_determinism(F, N, Det) :- det_analysis_cache(F, N, Det0), !, Det = Det0.
body_determinism(F, _, det) :- catch(b_getval('$det_stack', St), _, St = []),
                               memberchk(F, St), !.
body_determinism(F, N, Det) :- catch(nb_getval(F, Metas0), _, Metas0 = []),
                               include(arity_meta(N), Metas0, Metas),
                               ( Metas == []
                                 -> Arity is N + 1,
                                    ( functor(H, F, Arity), predicate_property(H, defined)
                                      -> Det = det ; Det = unspecified )
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
deterministic_expr([once, _], ok) :- !.
deterministic_expr([quote, _], ok) :- !.
deterministic_expr([eval, _], unknown(dynamic_eval)) :- !.
deterministic_expr([reduce, _], unknown(dynamic_reduce)) :- !.
deterministic_expr([call, Expr], Result) :- !, deterministic_call_expr(Expr, Result).
deterministic_expr([superpose|_], nondeterministic(superpose)) :- !.
deterministic_expr([match|_], nondeterministic(match)) :- !.
deterministic_expr([hyperpose|_], nondeterministic(hyperpose)) :- !.
deterministic_expr([translatePredicate|_], nondeterministic(translatePredicate)) :- !.
deterministic_expr([if, Cond, Then], Result) :- !, combine_determinism_list([Cond, Then], Result).
deterministic_expr([if, Cond, Then, Else], Result) :- !, combine_determinism_list([Cond, Then, Else], Result).
deterministic_expr([progn|Exprs], Result) :- !, combine_determinism_list(Exprs, Result).
deterministic_expr([prog1|Exprs], Result) :- !, combine_determinism_list(Exprs, Result).
deterministic_expr([let, Pat, Val, In], Result) :- !, pattern_then_exprs(Pat, [Val, In], Result).
deterministic_expr([chain, Pat, Val, In], Result) :- !, pattern_then_exprs(Pat, [Val, In], Result).
deterministic_expr(['let*', Binds, Body], Result) :- !, binds_and_body_determinism(Binds, Body, Result).
deterministic_expr([sealed, _, Expr], Result) :- !, deterministic_expr(Expr, Result).
deterministic_expr(['forall', _, _], ok) :- !.
deterministic_expr(['foldall', _, _, _], ok) :- !.
deterministic_expr(['foldl-atom', List, Init, _, _, Body], Result) :- !, combine_determinism_list([List, Init, Body], Result).
deterministic_expr(['map-atom', List, _, Body], Result) :- !, combine_determinism_list([List, Body], Result).
deterministic_expr(['filter-atom', List, _, Cond], Result) :- !, combine_determinism_list([List, Cond], Result).
deterministic_expr(['|->', _, _], ok) :- !.
deterministic_expr([case, KeyExpr, PairsExpr], Result) :- !, case_expr_determinism(KeyExpr, PairsExpr, Result).
deterministic_expr([Head|Args], Result) :- ( atomic(Head), ( \+ atom(Head) ; \+ fun(Head) )
                                           ; is_list(Head) ), !,
                                           combine_determinism_list([Head|Args], Result).
deterministic_expr([Head|Args], Result) :- atom(Head), !, deterministic_call_expr([Head|Args], Result).
deterministic_expr([Head|_], unknown(dynamic_head(Head))).

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

det_atom_evidence(F2, M) :- ( catch(fn_determinism(F2, M, det), _, fail) -> true
                            ; body_determinism(F2, M, det) ).

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
                                                       combine_det_results(KeyResult, R2, Result) ).

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
         with_form_location(Line, Str, check_det_exhaustive(Consts, F, N, Heads))
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
%LIMITATION (the same one union_member_excluded/3 documents): the set is read
%as it stands right now. A constructor for T declared in a LATER file would
%invalidate an "unmatched constructor" verdict made here, and the rejected
%file is not revisited. Declare a type's constructors before the -[det]->
%functions that match on it.
domain_keys('Bool', _, [key(true, 0), key(false, 0)]) :- !.
domain_keys(T, Consts, Keys) :- atom(T), declared_newtype(T, R), !, domain_keys(R, Consts, Keys).
domain_keys(T, Consts, Keys) :- atom(T), \+ wildcard_type(T), \+ primitive_type(T),
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

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


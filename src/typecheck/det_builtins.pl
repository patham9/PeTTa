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

%The builtin table is the checker's OWN bookkeeping about a symbol's result
%count, and where it has an entry that entry OVERRIDES any determinism derived
%from the program's own declaration. table_det_verdict/3 is that lookup, and
%the override it implements is shared by every site that needs a symbol's
%effective determinism, so the three cannot drift: a direct call
%(function_call_determinism/3), a value-position arrow head (value_arrow_head/4),
%and the oracle's wrapping decision (oracle_det_believed/3). The atom(F) guard
%keeps a still-unbound F from enumerating the table by binding itself to a
%builtin name; every caller here already passes a bound atom.
table_det_verdict(F, N, Det) :- atom(F), builtin_call_determinism(F, N, Det).

%The override applied to an ALREADY-COMPUTED fallback determinism (value_arrow_head
%and oracle_det_believed hold theirs in hand). function_call_determinism keeps
%the table check first and its fallback lazy, because its fallback is a goal that
%may legitimately have no answer for a table-only builtin.
table_det_override(F, N, Fallback, Det) :- ( table_det_verdict(F, N, DetB) -> Det = DetB ; Det = Fallback ).

function_call_determinism(F, N, Det) :- table_det_verdict(F, N, Det), !.
function_call_determinism(F, N, Det) :- catch(fn_determinism(F, N, Det0), _, fail),
                                        Det0 \== unspecified, !, Det = Det0.
function_call_determinism(F, N, Det) :- body_determinism(F, N, Det).


:- ensure_loaded([parser, translator, filereader]).

%%%%%%%%%% Standard Library for MeTTa %%%%%%%%%%

%%% Let bindings: %%%
'let*'([], B, B).
'let*'([[V,Val]|Rs], B, Out) :- V = Val, 'let*'(Rs, B, Out).
let(V,Val,In,Out) :- 'let*'([[V,Val]], In, Out).

%%% Arithmetic & Comparison: %%%
'+'(A,B,R)  :- R is A + B.
'-'(A,B,R)  :- R is A - B.
'*'(A,B,R)  :- R is A * B.
'/'(A,B,R)  :- R is A / B.
'%'(A,B,R)  :- R is A mod B.
'<'(A,B,R)  :- (A<B -> R=true ; R=false).
'>'(A,B,R)  :- (A>B -> R=true ; R=false).
'=='(A,B,R) :- (A==B -> R=true ; R=false).
'='(A,B,R) :- (A=B -> R=true ; R=false).
'<='(A,B,R) :- (A =< B -> R=true ; R=false).
'>='(A,B,R) :- (A => B -> R=true ; R=false).
min(A,B,R)  :- R is min(A,B).
max(A,B,R)  :- R is max(A,B).

%%% Boolean Logic: %%%
and(true,  X, X).
and(false, _, false).
or( false, X, X).
or( true,  _, true).
not(true,  false).
not(false, true).

%%% Nondeterminism: %%%
superpose(L,X) :- member(X,L).
empty(_) :- fail.

%%% Lists / Tuples: %%%
'car-atom'([H|_], H).
'cdr-atom'([_|T], T).
memberfast(X, List, true) :- memberchk(X, List), !.
memberfast(_, _, false).
excludefast(A,L,R) :- exclude(==(A), L, R).

%%% Diagnostics / Testing: %%%
'trace!'(In, Content, Out) :- format('~w~n', [In]), Out = Content.
test(A,B,R) :- (A==B -> E='✅' ; E='❌'),
               format(string(R), "is ~w, should ~w. ~w", [A,B,E]).

%%% Spaces: %%%

%Arity expander for JIT-indexing-efficient representation of space entries:
ensure_dynamic_arity(Space,Arity) :- ( current_predicate(Space/Arity)
                                       -> true ; dynamic(Space/Arity) ).

%Add an atom to the space:
'add-atom'(Space, [Rel|Args], true) :- length(Args, N), Arity is N + 2,
                                       ensure_dynamic_arity(Space, Arity),
                                       Term =.. [Space, Rel | Args],
                                       assertz(Term).

%Remove all same atoms:
'remove-atom'(Space, [Rel|Args], true) :- length(Args, N), Arity is N + 2,
                                          ensure_dynamic_arity(Space, Arity),
                                          Term =.. [Space, Rel | Args],
                                          ( clause(Term, true)
                                            -> retractall(Term) ).

%Match only a single instance, existential check:
'match-once'(Space, Pattern, OutPattern, Result) :- once(match(Space, Pattern, OutPattern, Result)).

%Function evaluation matches, where the unification returned true, so it unified:
match('&self', true, Arg2, Result) :- Result=Arg2.

%Match for pattern:
match(Space, [Rel|PatArgs], OutPattern, Result) :- Term =.. [Space, Rel | PatArgs],
                                                   Term, Result = OutPattern.

%Get all atoms in space, irregard of arity:
'get-atoms'(Space, Pattern) :- current_predicate(Space/Arity),
                               functor(Head, Space, Arity),
                               clause(Head, true),
                               Head =.. [Space | Pattern].


:- use_module(library(janus)).

'py-atom'(Path, ['py-atom', Path]).
'py-dot'(Base, Attr, ['py-dot', Base, Attr]).
'py-list'(In, Out) :- Out = In.    % Prolog list is fine for torch


%%% Registration: %%%
:- dynamic fun/1.
register_fun(N)   :- (fun(N)->true ; assertz(fun(N))).
unregister_fun(N) :- retractall(fun(N)).
:- maplist(register_fun, [superpose, empty, let, 'let*', '+','-','*','/', '%', min, max,
                          '<','>','==', '=', '<=', '>=', and, or, not, 'car-atom', 'cdr-atom', 'trace!', test,
                          append, length, sort, msort, memberfast, excludefast, list_to_set,
                          'add-atom', 'remove-atom', 'get-atoms', 'match', 'match-once',
                          'py-atom', 'py-dot', 'py-list']).

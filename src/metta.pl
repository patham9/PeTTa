:- use_module(library(clpfd)).
:- use_module(library(clpr)).

% --- Dispatchers ---
arith(Op, A, B, R) :-
    (   integer(A), integer(B)
    ->  arith_fd(Op, A, B, R)
    ;   arith_fr(Op, A, B, R)
    ).

cmp(Op, A, B, R) :-
    (   integer(A), integer(B)
    ->  cmp_fd(Op, A, B, R)
    ;   cmp_fr(Op, A, B, R)
    ).

% --- CLPFD arithmetic ---
arith_fd(+, A, B, R) :- R #= A + B.
arith_fd(-, A, B, R) :- R #= A - B.
arith_fd(*, A, B, R) :- R #= A * B.
arith_fd(div, A, B, R) :- R #= A div B.
arith_fd(mod, A, B, R) :- R #= A mod B.
arith_fd(min, A, B, R) :- R #= min(A,B).
arith_fd(max, A, B, R) :- R #= max(A,B).

% --- CLPR arithmetic ---
arith_fr(+, A, B, R) :- {R = A + B}.
arith_fr(-, A, B, R) :- {R = A - B}.
arith_fr(*, A, B, R) :- {R = A * B}.
arith_fr(/, A, B, R) :- {R = A / B}.
arith_fr(min, A, B, R) :- {R = min(A,B)}.
arith_fr(max, A, B, R) :- {R = max(A,B)}.

% --- CLPFD comparisons ---
cmp_fd(<,  A, B, true)  :- A #<  B, !.
cmp_fd(<,  _, _, false).
cmp_fd(>,  A, B, true)  :- A #>  B, !.
cmp_fd(>,  _, _, false).
cmp_fd(=,  A, B, true)  :- A #=  B, !.
cmp_fd(=,  _, _, false).
cmp_fd(\=, A, B, true)  :- A #\= B, !.
cmp_fd(\=, _, _, false).

% --- CLPR comparisons ---
cmp_fr(<,  A, B, true)  :- {A <  B}, !.
cmp_fr(<,  _, _, false).
cmp_fr(>,  A, B, true)  :- {A >  B}, !.
cmp_fr(>,  _, _, false).
cmp_fr(=,  A, B, true)  :- {A =  B}, !.
cmp_fr(=,  _, _, false).
cmp_fr(\=, A, B, true)  :- {A =\= B}, !.
cmp_fr(\=, _, _, false).

% --- Convenience wrappers ---
'+'(A,B,R)   :- arith(+, A, B, R).
'-'(A,B,R)   :- arith(-, A, B, R).
'*'(A,B,R)   :- arith(*, A, B, R).
'/'(A,B,R)   :- arith(/, A, B, R).
'%'(A,B,R)   :- arith(mod, A, B, R).
min(A,B,R)   :- arith(min, A, B, R).
max(A,B,R)   :- arith(max, A, B, R).

'<'(A,B,R)   :- cmp(<, A, B, R).
'>'(A,B,R)   :- cmp(>, A, B, R).
'=='(A,B,R)  :- (A==B -> R=true ; R=false).
'='(A,B,R) :- (A=B -> R=true ; R=false).

:- ensure_loaded([parser, translator, filereader]).

%%%%%%%%%% Standard Library for MeTTa %%%%%%%%%%

%Let bindings:
'let*'([], B, B).
'let*'([[V,Val]|Rs], B, Out) :- V = Val, 'let*'(Rs, B, Out).
let(V,Val,In,Out) :- 'let*'([[V,Val]], In, Out).

%Boolean Logic:
and(true,  X, X).
and(false, _, false).
or( false, X, X).
or( true,  _, true).
not(true,  false).
not(false, true).

%Nondeterminism:
superpose(L,X) :- member(X,L).
empty(_) :- fail.

%Lists/Tuples:
'car-atom'([H|_], H).
'cdr-atom'([_|T], T).
memberfast(X, List, true) :- memberchk(X, List), !.
memberfast(_, _, false).
excludefast(A,L,R) :- exclude(==(A), L, R).

%Diagnostics / Testing:
'trace!'(In, Content, Out) :- format('~w~n', [In]), Out = Content.
test(A,B,R) :- (A==B -> E='✅' ; E='❌'),
               format(string(R), "is ~w, should ~w. ~w", [A,B,E]).

%%% Spaces %%%

ensure_dynamic_arity(Space,Arity) :- ( current_predicate(Space/Arity)
                                       -> true ; dynamic(Space/Arity) ).

'add-atom'(Space, [Rel|Args], true) :- length(Args, N), Arity is N + 2,
                                       ensure_dynamic_arity(Space, Arity),
                                       Term =.. [Space, Rel | Args],
                                       assertz(Term).

'remove-atom'(Space, [Rel|Args], Result) :- length(Args, N), Arity is N + 2,
                                            ensure_dynamic_arity(Space, Arity),
                                            Term =.. [Space, Rel | Args],
                                            ( clause(Term, true)
                                              -> retractall(Term),
                                                 Result = true
                                               ; Result = false ).

%Function evaluation matches, where the unification returned true, so it unified:
match('&self', true, Arg2, Result) :- Result=Arg2.

%Match for pattern:
match(Space, [Rel|PatArgs], OutPattern, Result) :- Term =.. [Space, Rel | PatArgs],
                                                   Term, Result = OutPattern.

%Registration:
:- dynamic fun/1.
register_fun(N)   :- (fun(N)->true ; assertz(fun(N))).
unregister_fun(N) :- retractall(fun(N)).
:- maplist(register_fun, [superpose, empty, let, 'let*', '+','-','*','/', '%', min, max,
                          '<','>','==', '=', and, or, not, 'car-atom', 'cdr-atom', 'trace!', test,
                          append, length, sort, msort, memberfast, excludefast, list_to_set,
                          'add-atom', 'remove-atom', 'match']).

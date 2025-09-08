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

%%% Spaces %%%

% one trie per Space / Arity
:- dynamic space_trie/3.  % space_trie(Space, Arity, Trie)

ensure_trie(Space, Arity, T) :- ( space_trie(Space, Arity, T)
                                  -> true
                                   ; trie_new(T),
                                     assertz(space_trie(Space, Arity, T)) ).

'add-atom'(Space, [Rel|Args], true) :- length(Args, N),
                                       Arity is N+2,
                                       ensure_trie(Space, Arity, T),
                                       Key =.. [Space, Rel|Args],
                                       trie_insert(T, Key).

'remove-atom'(Space, [Rel|Args], Result) :- length(Args, N),
                                            Arity is N+2,
                                            ensure_trie(Space, Arity, T),
                                            Key =.. [Space, Rel|Args],
                                            ( trie_delete(T, Key)
                                              -> Result = true
                                               ; Result = false ).

%Function evaluation matches, where the unification returned true, so it unified:
match('&self', true, Arg2, Result) :- Result=Arg2.

% exact and prefix matches
match(Space, [Rel|PatArgs], Out, Result) :- length(PatArgs, N),
                                            Arity is N+2,
                                            space_trie(Space, Arity, T),
                                            Key =.. [Space, Rel|PatArgs],
                                            trie_gen(T, Key, _),
                                            Result = Out.

%Registration:
:- dynamic fun/1.
register_fun(N)   :- (fun(N)->true ; assertz(fun(N))).
unregister_fun(N) :- retractall(fun(N)).
:- maplist(register_fun, [superpose, empty, let, 'let*', '+','-','*','/', '%', min, max,
                          '<','>','==', '=', and, or, not, 'car-atom', 'cdr-atom', 'trace!', test,
                          append, length, sort, msort, memberfast, excludefast, list_to_set,
                          'add-atom', 'remove-atom', 'match']).

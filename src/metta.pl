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
:- use_module(library(rocksdb)).
:- dynamic kb_db/1.

% lazy DB init
ensure_db(DB) :- kb_db(DB), !.
ensure_db(DB) :- shell('rm -rf kb.rocks'),
                 rocks_open('kb.rocks', DB, [create_if_missing(true)]),
                 asserta(kb_db(DB)).

% ASCII Unit Separator (31) to keep prefix groups contiguous
sep(S) :- char_code(S, 31).

encode_key(Space, Rel, Args, Key) :- sep(SEP),
                                     maplist(term_string, [Space,Rel|Args], Parts),
                                     atomic_list_concat(Parts, SEP, Key).

prefix_key(Space, Rel, Prefix) :- sep(SEP),
                                  maplist(term_string, [Space,Rel], [SS,RS]),
                                  atomic_list_concat([SS,RS,''], SEP, Prefix).

decode_key(Key, Space, Rel, Args) :- sep(SEP),
                                     atomic_list_concat(Parts, SEP, Key),
                                     Parts = [SS,RS|AS],
                                     maplist(read_term_from_atom_, [SS,RS|AS], [Space,Rel|Args]).
read_term_from_atom_(A,T) :- read_term_from_atom(A,T,[]).

'add-atom'(Space, [Rel|Args], true) :- ensure_db(DB),
                                       encode_key(Space, Rel, Args, K),
                                       rocks_put(DB, K, v).

'remove-atom'(Space, [Rel|Args], Result) :- ensure_db(DB),
                                            encode_key(Space, Rel, Args, K),
                                            ( rocks_get(DB, K, _) -> rocks_delete(DB, K), Result = true
                                            ; Result = false ).

%Function evaluation matches, where the unification returned true, so it unified:
match('&self', true, Arg2, Result) :- Result=Arg2.

%Match for pattern
match(Space, [Rel|PatArgs], OutPattern, Result) :- ensure_db(DB),
                                                   prefix_key(Space, Rel, Prefix),
                                                   rocks_enum_prefix(DB, Suffix, _V, Prefix, []),
                                                   atom_concat(Prefix, Suffix, FullKey),
                                                   decode_key(FullKey, _S, _R, Args),
                                                   PatArgs = Args,
                                                   Result = OutPattern.

iter_prefixed(It, Prefix, Key) :- rocks_iterator_next(It, K, _V),
                                  ( sub_atom(K, 0, _, _, Prefix)
                                    -> Key = K
                                     ;  iter_prefixed(It, Prefix, Key) ).

%Registration:
:- dynamic fun/1.
register_fun(N)   :- (fun(N)->true ; assertz(fun(N))).
unregister_fun(N) :- retractall(fun(N)).
:- maplist(register_fun, [superpose, empty, let, 'let*', '+','-','*','/', '%', min, max,
                          '<','>','==', '=', and, or, not, 'car-atom', 'cdr-atom', 'trace!', test,
                          append, length, sort, msort, memberfast, excludefast, list_to_set,
                          'add-atom', 'remove-atom', 'match']).

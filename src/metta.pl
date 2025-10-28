:- current_prolog_flag(argv, Argv),
   ( member(mork, Argv) -> ensure_loaded([parser, translator, filereader, morkspaces, spaces])
                         ; ensure_loaded([parser, translator, filereader, spaces])).

%%%%%%%%%% Standard Library for MeTTa %%%%%%%%%%

%%% Let bindings: %%%
'let*'([], B, B).
'let*'([[V,Val]|Rs], B, Out) :- V = Val, 'let*'(Rs, B, Out).
let(V, Val, In, Out) :- 'let*'([[V,Val]], In, Out).

%% Representation conversion: %%
id(X, X).
repr(Term, R) :- swrite(Term, R).
repra(Term, R) :- term_to_atom(Term, R).

%%% Arithmetic & Comparison: %%%
'+'(A,B,R)  :- R is A + B.
'-'(A,B,R)  :- R is A - B.
'*'(A,B,R)  :- R is A * B.
'/'(A,B,R)  :- R is A / B.
'%'(A,B,R)  :- R is A mod B.
'<'(A,B,R)  :- (A<B -> R=true ; R=false).
'>'(A,B,R)  :- (A>B -> R=true ; R=false).
'=='(A,B,R) :- (A==B -> R=true ; R=false).
'='(A,B,R) :-  (A=B -> R=true ; R=false).
'=?'(A,B,R) :- (\+ \+ A=B -> R=true ; R=false).
'=alpha'(A,B,R) :- (A =@= B -> R=true ; R=false).
'=@='(A,B,R) :- (A =@= B -> R=true ; R=false).
'<='(A,B,R) :- (A =< B -> R=true ; R=false).
'>='(A,B,R) :- (A >= B -> R=true ; R=false).
min(A,B,R)  :- R is min(A,B).
max(A,B,R)  :- R is max(A,B).
exp(Arg,R) :- R is exp(Arg).
:- use_module(library(clpfd)).
'#+'(A, B, R) :- R #= A + B.
'#-'(A, B, R) :- R #= A - B.
'#*'(A, B, R) :- R #= A * B.
'#div'(A, B, R) :- R #= A div B.
'#//'(A, B, R) :- R #= A // B.
'#mod'(A, B, R) :- R #= A mod B.
'#min'(A, B, R) :- R #= min(A,B).
'#max'(A, B, R) :- R #= max(A,B).
'#<'(A, B, true)  :- A #< B, !.
'#<'(_, _, false).
'#>'(A, B, true)  :- A #> B, !.
'#>'(_, _, false).
'#='(A, B, true)  :- A #= B, !.
'#='(_, _, false).
'#\\='(A, B, true)  :- A #\= B, !.
'#\\='(_, _, false).
'pow-math'(A, B, Out) :- Out is A ** B.
'sqrt-math'(A, Out)   :- Out is sqrt(A).
'abs-math'(A, Out)    :- Out is abs(A).
'log-math'(Base, X, Out) :- Out is log(X) / log(Base).
'trunc-math'(A, Out)  :- Out is truncate(A).
'ceil-math'(A, Out)   :- Out is ceil(A).
'floor-math'(A, Out)  :- Out is floor(A).
'round-math'(A, Out)  :- Out is round(A).
'sin-math'(A, Out)  :- Out is sin(A).
'cos-math'(A, Out)  :- Out is cos(A).
'tan-math'(A, Out)  :- Out is tan(A).
'asin-math'(A, Out) :- Out is asin(A).
'acos-math'(A, Out) :- Out is acos(A).
'atan-math'(A, Out) :- Out is atan(A).
'isnan-math'(A, Out) :- ( A =:= A -> Out = false ; Out = true ).
'isinf-math'(A, Out) :- ( A =:= 1.0Inf ; A =:= -1.0Inf -> Out = true ; Out = false ).
'min-atom'(List, Out) :- min_list(List, Out).
'max-atom'(List, Out) :- max_list(List, Out).

%%% Boolean Logic: %%%
and(true,  X, X).
and(false, _, false).
or( false, X, X).
or( true,  _, true).
not(true,  false).
not(false, true).

%%% Expression size %%%
'size-atom'(List, Size) :- length(List, Size).

%%% Nondeterminism: %%%
superpose(L,X) :- member(X,L).
empty(_) :- fail.

%%% Lists / Tuples: %%%
'cons-atom'(H, T, [H|T]).
'decons-atom'([H|T], [H|[T]]).
'first-from-pair'([A, _], A).
'second-from-pair'([_, A], A).
'unique-atom'(A, B) :- list_to_set(A, B).
'car-atom'([H|_], H).
'cdr-atom'([_|T], T).
decons([H|T], [H|[T]]).
cons(H, T, [H|T]).
'index-atom'(List, Index, Elem) :- nth0(Index, List, Elem).
'is-member'(X, List, true) :- member(X, List).
'is-member'(X, List, false) :- \+ member(X, List).
'exclude-item'(A, L, R) :- exclude(==(A), L, R).

%Multisets:
'subtraction-atom'([], _, []).
'subtraction-atom'([H|T], B, Out) :- ( select(H, B, BRest) -> 'subtraction-atom'(T, BRest, Out)
                                                            ; Out = [H|Rest],
                                                              'subtraction-atom'(T, B, Rest) ).
'union-atom'(A, B, Out) :- append(A, B, Out).
'intersection-atom'(A, B, Out) :- intersection(A, B, Out).

%%% Type system: %%%
get_function_type([F,Arg], T) :- match('&self', [':',F,['->',A,B]], _, _),
                                 'get-type'(Arg, A),
                                 T = B.

'get-type'(X, 'Number')   :- number(X), !.
'get-type'(X, 'Variable') :- var(X), !.
'get-type'(X, 'String')   :- string(X), !.
'get-type'(true, 'Bool')  :- !.
'get-type'(false, 'Bool') :- !.
'get-type'(X, T) :- get_function_type(X,T).
'get-type'(X, T) :- \+ get_function_type(X, _),
                    is_list(X),
                    maplist('get-type', X, T).
'get-type'(X, T) :- match('&self', [':',X,T], T, _).

'get-metatype'(X, 'Variable') :- var(X), !.
'get-metatype'(X, 'Grounded') :- number(X), !.
'get-metatype'(X, 'Grounded') :- string(X), !.
'get-metatype'(true,  'Grounded') :- !.
'get-metatype'(false, 'Grounded') :- !.
'get-metatype'(X, 'Grounded') :- atom(X), fun(X), !.  % e.g., '+' is a registered fun/1
'get-metatype'(X, 'Expression') :- is_list(X), !.     % e.g., (+ 1 2), (a b)
'get-metatype'(X, 'Symbol') :- atom(X), !.            % e.g., a
'is-var'(A,R) :- (var(A) -> R=true ; R=false).
'is-expr'(A,R) :- (is_list(A) -> R=true ; R=false).

%Helper functions
member_with_pred(Element, [Head|_], Pred) :- call(Pred, Element, Head, true).
member_with_pred(Element, [_|Tail], Pred) :- member_with_pred(Element, Tail, Pred).

% Convert a list to a set using the given equality predicate
list_to_set(Pred, List, Set) :- list_to_set_helper(Pred, List, [], Set).
list_to_set_helper(_Pred, [], Acc, Acc).
list_to_set_helper(Pred, [H|T], Acc, Set) :- ( member_with_pred(H, Acc, Pred)
                                               -> list_to_set_helper(Pred, T, Acc, Set)
                                                ; list_to_set_helper(Pred, T, [H|Acc], Set) ).

%Set based Union
union(Pred, List1, List2, Result) :- list_to_set(Pred, List1, Set1),
                                     list_to_set(Pred, List2, Set2), !,
                                     union_helper(Pred, Set1, Set2, Result).

union_helper(_Pred, [], [], []) :- !.
union_helper(_Pred, List1, [], List1) :- !.
union_helper(Pred, List1, [Head2|Tail2], [Head2|Output]) :- \+ member_with_pred(Head2, List1, Pred),
                                                               union_helper(Pred, List1, Tail2, Output).
union_helper(Pred, List1, [Head2|Tail2], Output) :- member_with_pred(Head2, List1, Pred),
                                                    union_helper(Pred, List1, Tail2, Output).

%List based Intersection
intersection([], _, []).
intersection(_, [], []).
intersection([H|T1], L2, [H|Res]) :- select(H, L2, R2), !,       
                                     intersection(T1, R2, Res).
intersection([_|T1], L2, Res) :- intersection(T1, L2, Res).

%List based Subtraction
subtract(_Pred, [], _, []).
subtract(Pred, [E|T], D, R) :- ( member_with_pred(E, D, Pred) -> subtract(Pred, T, D, R)
                                                               ; R = [E|R1],
                                                                 subtract(Pred, T, D, R1) ).

%%% Higher-order predicates: %%%
'fold-flat'([], Acc, _Combiner, Acc).
'fold-flat'([Head|Tail], Acc, Combiner, Result) :- call(Combiner, Acc, Head, NewAcc),  % Apply Combiner(Acc, Head, NewAcc)
                                                   'fold-flat'(Tail, NewAcc, Combiner, Result).

'fold-nested'([], Acc, _Combiner, Acc).
'fold-nested'(A, Acc, Combiner, Result) :- atom(A),
                                           call(Combiner, Acc, A, Result).

'fold-nested'([Head|Tail], Acc, Combiner, Result) :- \+ is_list(Head),
                                                     call(Combiner, Acc, Head, NewAcc),  % Apply Combiner(Acc, Head, NewAcc)
                                                     'fold-nested'(Tail, NewAcc, Combiner, Result).

'fold-nested'([Head|Tail], Acc, Combiner, Result) :- is_list(Head),
                                                     'fold-nested'(Head, Acc, Combiner, NewAcc),
                                                     'fold-nested'(Tail, NewAcc, Combiner, Result).

'map-flat'([], _Mapper, []).
'map-flat'([Head|Tail], Mapper, [NewHead|NewTail]) :- call(Mapper, Head, NewHead),
                                                      'map-flat'(Tail, Mapper, NewTail).

'map-nested'([], _Mapper, []).
'map-nested'(Atom, Mapper, Result) :- atom(Atom),
                                      call(Mapper, Atom, Result).
'map-nested'([Head|Tail], Mapper, [NewHead|NewTail]) :- is_list(Head),
                                                        'map-nested'(Head, Mapper, NewHead),
                                                        'map-nested'(Tail, Mapper, NewTail).

'map-nested'([Head|Tail], Mapper, [NewHead|NewTail]) :- \+ is_list(Head),
                                                        call(Mapper, Head, NewHead),
                                                        'map-nested'(Tail, Mapper, NewTail).

%%% Diagnostics / Testing: %%%
'println!'(Arg, true) :- swrite(Arg, RArg),
                         format('~w~n', [RArg]).

'readln!'(Out) :- read_line_to_string(user_input, Str),
                  sread(Str, Out).

'trace!'(In, Content, Content) :- swrite(In,R),
                                  format('~w~n', [R]).

test(A,B,true) :- (A =@= B -> E = '✅' ; E = '❌'),
                  swrite(A, RA),
                  swrite(B, RB),
                  format("is ~w, should ~w. ~w ~n", [RA, RB, E]).

assert(Goal, true) :- ( call(Goal) -> true
                                    ; swrite(Goal, RG),
                                      format("Assertion failed: ~w~n", [RG]),
                                      halt(1) ).

%%% Python bindings: %%%
'py-call'(SpecList, Result) :- 'py-call'(SpecList, Result, []).
'py-call'([Spec|Args], Result, Opts) :- ( string(Spec) -> atom_string(A, Spec) ; A = Spec ),
                                        must_be(atom, A),
                                        ( sub_atom(A, 0, 1, _, '.')         % ".method"
                                          -> sub_atom(A, 1, _, 0, Fun),
                                             Args = [Obj|Rest],
                                             ( Rest == []
                                               -> compound_name_arguments(Meth, Fun, [])
                                                ; Meth =.. [Fun|Rest] ),
                                             py_call(Obj:Meth, Result, Opts)
                                           ; atomic_list_concat([M,F], '.', A) % "mod.fun"
                                             -> ( Args == []
                                                  -> compound_name_arguments(Call0, F, [])
                                                   ; Call0 =.. [F|Args] ),
                                                py_call(M:Call0, Result, Opts)
                                              ; ( Args == []                      % bare "fun"
                                                  -> compound_name_arguments(Call0, A, [])
                                                   ; Call0 =.. [A|Args] ),
                                                py_call(builtins:Call0, Result, Opts) ).

%%% States: %%%
'bind!'(A, ['new-state', B], C) :- 'change-state!'(A, B, C).
'change-state!'(Var, Value, true) :- nb_setval(Var, Value).
'get-state'(Var, Value) :- nb_getval(Var, Value).

%%% Eval: %%%
eval(C, Out) :- translate_expr(C, Goals, Out),
                call_goals(Goals).

call_goals([]).
call_goals([G|Gs]) :- call(G), 
                      call_goals(Gs).

%%% Registration: %%%
'import!'('&self', File, true) :- atom_string(File, SFile),
                                  working_dir(Base),
                                  atomic_list_concat([Base, '/', SFile, '.metta'], Path),
                                  load_metta_file(Path, default).

:- dynamic fun/1.
register_fun(N) :- (fun(N) -> true ; assertz(fun(N))).
unregister_fun(N/Arity) :- retractall(fun(N)),
                           abolish(N, Arity).

:- maplist(register_fun, [superpose, empty, let, 'let*', '+','-','*','/', '%', min, max, 'change-state!', 'get-state', 'bind!',
                          '<','>','==', '=', '=?', '<=', '>=', and, or, not, sqrt, exp, log, cos, sin,
                          'first-from-pair', 'second-from-pair', 'car-atom', 'cdr-atom', 'unique-atom',
                          repr, repra, 'println!', 'readln!', 'trace!', test, assert, 'mm2-exec',
                          foldl, append, length, sort, msort, 'is-member', 'exclude-item', list_to_set, maplist, eval, reduce, 'import!',
                          'add-atom', 'remove-atom', 'get-atoms', match, 'is-var', 'is-expr', 'get-mettatype',
                          decons, 'decons-atom', 'fold-flat', 'fold-nested', 'map-flat', 'map-nested', union, intersection, subtract,
                          'py-call', 'get-type', 'get-metatype', '=alpha', concat, sread, cons, reverse,
                          '#+','#-','#*','#div','#//','#mod','#min','#max','#<','#>','#=','#\\=',
                          'union-atom', 'cons-atom', 'intersection-atom', 'subtraction-atom', 'index-atom', id,
                          'pow-math', 'sqrt-math', 'abs-math', 'log-math', 'trunc-math', 'ceil-math',
                          'floor-math', 'round-math', 'sin-math', 'cos-math', 'tan-math', 'asin-math',
                          'acos-math', 'atan-math', 'isnan-math', 'isinf-math', 'min-atom', 'max-atom', 'size-atom']).
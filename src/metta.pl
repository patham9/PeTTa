:- ensure_loaded([parser, translator, filereader, spaces]).

%%%%%%%%%% Standard Library for MeTTa %%%%%%%%%%

%%% Let bindings: %%%
'let*'([], B, B).
'let*'([[V,Val]|Rs], B, Out) :- V = Val, 'let*'(Rs, B, Out).
let(V, Val, In, Out) :- 'let*'([[V,Val]], In, Out).

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
'exp-math'(Arg,R) :- R is exp(Arg).

%Custom Maths function
'cos-math'(Arg,R) :- R is cos(Arg).
'sin-math'(Arg,R) :- R is sin(Arg).
'tan-math'(Arg,R) :- R is tan(Arg).
'acos-math'(Arg,R) :- R is acos(Arg).
'asin-math'(Arg,R) :- R is asin(Arg).
'atan-math'(Arg,R) :- R is atan(Arg).

'sqrt-math'(Arg, R) :-
    R is sqrt(Arg).

'abs-math'(Arg, R) :-
    ( Arg > 0 ->
        R is Arg
    ; Arg < 0 ->
        R is -Arg
    ;
        R is 0
    ).
'log-math'(Arg, R) :-
    R is log(Arg).

'trunc-math'(A, R) :-
    R is truncate(A).

'ceil-math'(Arg, R) :-
    R is ceiling(Arg).

'floor-math'(Arg, R) :-
    R is floor(Arg).

'round-math'(Arg, R) :-
    R is round(Arg).

'isnan-math'(X, true) :- X \= X, !.
'isnan-math'(_, false).

'isinf-math'(X, true) :- (X =:= 1.0Inf ; X =:= -1.0Inf), !.
'isinf-math'(_, false).
'random-int'(Min, Max, R) :- random_between(Min, Max, R).

'random-float'(Min, Max, R) :- 
      random(X), 
      R is X * (Max - Min) + Min.
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

'is-function'([->, _, _], true) :- !.
'is-function'(_, false).

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
intersection(_Pred, [], _, []) :- !.
intersection(_Pred, _, [], []) :- !.
intersection(Pred, [Head1|Tail1], List2, [Head1|Output]) :- member_with_pred(Head1, List2, Pred),
                                                            intersection(Pred, Tail1, List2, Output).
intersection(Pred, [Head1|Tail1], List2, Output) :- \+ member_with_pred(Head1, List2, Pred),
                                                    intersection(Pred, Tail1, List2, Output).

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
repr(Term,R) :- swrite(Term, R).

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

%%% Registration: %%%
'import!'('&self', File, true) :- atom_string(File, SFile),
                                  working_dir(Base),
                                  atomic_list_concat([Base, '/', SFile, '.metta'], Path),
                                  load_metta_file(Path, default).

:- dynamic fun/1.
register_fun(N) :- (fun(N) -> true ; assertz(fun(N))).
unregister_fun(N/Arity) :- retractall(fun(N)),
                           abolish(N, Arity).

:- maplist(register_fun, [superpose, empty, let, 'let*', '+','-','*','/', '%', min, max,
                          '<','>','==', '=', '=?', '<=', '>=', and, or, not, 'sqrt-math', 'exp-math', 'log-math', 'cos-math', 'sin-math', 'tan-math',
                          'car-atom', 'cdr-atom', repr, 'println!', 'readln!', 'trace!', test, assertEqual,
                          append, 'size-atom', sort, msort, memberfast, excludefast, list_to_set, maplist, 'import!',
                          'add-atom', 'remove-atom', 'get-atoms', match, 'is-var', 'is-expr', 'get-mettatype',
                          decons, 'decons-atom', 'fold-flat', 'fold-nested', 'map-flat', 'map-nested', union, intersection, subtract,
                          'py-call', 'get-type', 'get-metatype', 'is-function', '=alpha', concat, sread, cons, reverse,
                          '#+','#-','#*','#div','#//','#mod','#min','#max','#<','#>','#=','#\\=',
                          'union-atom', 'cons-atom', 'intersection-atom', 'subtraction-atom', 'index-atom', id,
                          'pow-math', 'sqrt-math', 'abs-math', 'log-math', 'trunc-math', 'ceil-math',
                          'floor-math', 'round-math', 'sin-math', 'cos-math', 'tan-math', 'asin-math',
                          'acos-math', 'atan-math', 'isnan-math', 'isinf-math', 'min-atom', 'max-atom']).
:- current_prolog_flag(argv, Argv),
   ( member(mork, Argv) -> ensure_loaded([parser, translator, filereader, morkspaces, spaces])
                         ; ensure_loaded([parser, translator, filereader, spaces])).
%%%%%%%%%% Standard Library for MeTTa %%%%%%%%%%

%%% Let bindings: %%%
'let*'([], B, B).
'let*'([[V,Val]|Rs], B, Out) :- V = Val, 'let*'(Rs, B, Out).
let(V, Val, In, Out) :- 'let*'([[V,Val]], In, Out).

%%% Chain: evaluate first arg, bind to variable (second), then evaluate third
chain(Eval, Var, After, Out) :- let(Var, Eval, After, Out).

%%% Arithmetic & Comparison: %%%
'+'(A,B,R)  :- R is A + B.
'-'(A,B,R)  :- R is A - B.
'*'(A,B,R)  :- R is A * B.
'/'(A,B,R)  :- R is A / B.
'%'(A,B,R)  :- R is A mod B.
'<'(A,B,R)  :- (A<B -> R=true ; R=false).
'>'(A,B,R)  :- (A>B -> R=true ; R=false).
'=='(A,B,R) :- (eq_comprehensive(A,B) -> R=true ; R=false).

% Comprehensive equality checking for == operator
% Handles int/float equality, atoms, lists, and nested structures
eq_comprehensive(A, B) :- 
    % Handle numeric equality (int and float with equal magnitude)
    (number(A), number(B)) -> 
        (A =:= B ; (float(A), integer(B), A =:= float(B)) ; (integer(A), float(B), float(A) =:= B)) ;
    % Handle atom equality
    (atom(A), atom(B)) -> 
        A == B ;
    % Handle string equality
    (string(A), string(B)) -> 
        A == B ;
    % Handle boolean equality
    (A == true, B == true) -> true ;
    (A == false, B == false) -> true ;
    % Handle list equality (recursive)
    (is_list(A), is_list(B)) -> 
        length(A, LenA), length(B, LenB), LenA == LenB,
        eq_lists(A, B) ;
    % Handle compound term equality
    (compound(A), compound(B)) -> 
        functor(A, F, Arity), functor(B, F, Arity),
        eq_args(0, Arity, A, B) ;
    % Handle variable equality
    (var(A), var(B)) -> 
        A == B ;
    % Default case: exact equality
    A == B.

% Helper predicate for list equality checking
eq_lists([], []).
eq_lists([H1|T1], [H2|T2]) :- 
    eq_comprehensive(H1, H2),
    eq_lists(T1, T2).

% Helper predicate for compound term argument equality checking
eq_args(N, N, _, _) :- !.
eq_args(I, Arity, A, B) :- 
    I < Arity,
    I1 is I + 1,
    arg(I1, A, ArgA),
    arg(I1, B, ArgB),
    eq_comprehensive(ArgA, ArgB),
    eq_args(I1, Arity, A, B).
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
'decons'([H|T], [H|[T]]).
cons(H, T, [H|T]).
memberfast(X, List, true) :- member(X, List).
memberfast(X, List, false) :- \+ member(X, List).
excludefast(A, L, R) :- exclude(==(A), L, R).

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

%Commonly used predicates:
'is-var'(A,R) :- (var(A) -> R=true ; R=false).
'is-expr'(A,R) :- (is_list(A) -> R=true ; R=false).
concat(List1, List2, Result) :- append(List1, List2, Result).

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
repr(Term, R) :- swrite(Term, R).
repra(Term, R) :- term_to_atom(Term, R).

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

assertEqual(A,B,true) :- A \== B,
                         swrite(A, RA),
                         swrite(B,RB),
                         format("expected: ~w~nGot: ~w~nTerminating program~n", [RB, RA]),
                         halt(1).

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

:- maplist(register_fun, [superpose, empty, let, 'let*', chain, '+','-','*','/', '%', min, max,
                          '<','>','==', '=', '=?', '<=', '>=', and, or, not, sqrt, exp, log, cos, sin,
                          'car-atom', 'cdr-atom', repr, repra, 'println!', 'readln!', 'trace!', test, assertEqual, 'mm2-exec',
                          foldl, append, length, sort, msort, memberfast, excludefast, list_to_set, maplist, eval, reduce, 'import!',
                          'add-atom', 'remove-atom', 'get-atoms', match, 'is-var', 'is-expr', 'get-mettatype',
                          decons, 'fold-flat', 'fold-nested', 'map-flat', 'map-nested', union, intersection, subtract,
                          unify, 'py-call', 'get-type', 'get-metatype', '=alpha','=@=', concat, sread, cons, reverse,
                          '#+','#-','#*','#div','#//','#mod','#min','#max','#<','#>','#=','#\\=']).

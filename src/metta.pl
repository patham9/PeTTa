%%%%%%%%%% Dependencies %%%%%%%%%%

:- module(metta, []).

:- dynamic working_dir/1.
:- dynamic arity/2.

:- autoload(library(uuid), [uuid/1]).
:- use_module(library(random), [random/1, random_between/3]).
:- use_module(library(janus), [py_call/3]).
:- use_module(library(error), [must_be/2]).
:- use_module(library(listing), [portray_clause/2]).
:- use_module(library(aggregate), [foldall/4]).
:- use_module(library(thread), [concurrent_and/2]).
:- use_module(library(lists),
              [ append/3,
                member/2,
                select/3,
                nth0/3,
                list_to_set/2,
                intersection/3,
                reverse/2,
                subtract/3,
                min_list/2,
                max_list/2,
                flatten/2
              ]).
:- use_module(library(yall), []).
:- use_module(library(apply),
              [ include/3,
                exclude/3,
                foldl/4, foldl/5, foldl/6, foldl/7,
                maplist/2, maplist/3, maplist/4, maplist/5
              ]).
:- use_module(library(apply_macros)).

ensure_fun_arity(F, Arity) :-
    ( arity(F, Arity) -> true ; assertz(arity(F, Arity)) ).

require_fun_arity(F, Arity) :-
    arity(F, Arity).

% Predicate dispatch helpers shared across the runtime.
fun_goal(F, Args, Goal) :-
    length(Args, Arity),
    require_fun_arity(F, Arity),
    Goal =.. [F|Args].

call_fun(F, Args) :-
    fun_goal(F, Args, Goal),
    call(Goal).

call_in_namespace(Goal) :-
    Goal =.. [F|Args],
    call_fun(F, Args).

maybe_load_module(Module) :-
    ( Module == metta ; Module == user ) -> true
    ; current_module(Module) -> true
    ; catch(use_module(library(Module)), _, fail) -> true
    ; catch(use_module(Module), _, fail) -> true
    ; true.

wrap_module_fun(Module, F, Arity) :-
    ( current_predicate(metta:F/Arity)
    -> ensure_fun_arity(F, Arity)
     ; functor(Head, F, Arity),
       Head =.. [F|Args],
       Goal =.. [F|Args],
       assertz((Head :- Module:Goal)),
       ensure_fun_arity(F, Arity) ).

import_from_namespace(Module, F) :-
    maybe_load_module(Module),
    setof(Arity, current_predicate(Module:F/Arity), Arities),
    forall(member(Arity, Arities), wrap_module_fun(Module, F, Arity)), !.

import_user_fun(F) :- import_from_namespace(user, F), !.
import_user_fun(_).


:- current_prolog_flag(argv, Argv),
   ( member(mork, Argv) -> ensure_loaded([parser, translator, filereader, morkspaces, spaces])
                         ; ensure_loaded([parser, translator, filereader, spaces])).

%%%%%%%%%% Standard Library for MeTTa %%%%%%%%%%

%%% Representation conversion: %%%
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
'!='(A,B,R) :- (A==B -> R=false ; R=true).
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

%%% Random Generators: %%%
'random-int'(Min, Max, Result) :- random_between(Min, Max, Result).
'random-int'('&rng', Min, Max, Result) :- random_between(Min, Max, Result).
'random-float'(Min, Max, Result) :- random(R), Result is Min + R * (Max - Min).
'random-float'('&rng', Min, Max, Result) :- random(R), Result is Min + R * (Max - Min).

normalize_module_term(Spec, Term) :-
    ( string(Spec)
    -> atom_string(Atom, Spec),
       ( sub_atom(Atom, 0, _, _, 'library/')
       -> sub_atom(Atom, 8, _, 0, LibAtom),
          Term =.. [library, LibAtom]
        ; Term = Atom )
    ; is_list(Spec), Spec = [Functor|Args]
    -> Term =.. [Functor|Args]
    ; Term = Spec ),
    ( atom(Term) ; compound(Term) ).

use_module(Spec, true) :-
    normalize_module_term(Spec, Term),
    system:use_module(metta:Term).

wrap_module_fun(Module, F/Arity) :-
    maybe_load_module(Module),
    ( current_predicate(metta:F/Arity)
    -> true
     ; functor(Head, F, Arity),
       Head =.. [F|Args],
       Goal =.. [F|Args],
       assertz((Head :- Module:Goal)) ),
    ensure_fun_arity(F, Arity).

%%% Boolean Logic: %%%
and(true,  X, X).
and(false, _, false).
or( false, X, X).
or( true,  _, true).
not(true,  false).
not(false, true).
xor(false, A, A).
xor(true, A, B) :- not(A, B).

%%% Nondeterminism: %%%
superpose(L,X) :- member(X,L).
empty(_) :- fail.

%%% Lists / Tuples: %%%
'cons-atom'(H, T, [H|T]).
'decons-atom'([H|T], [H|[T]]).
'first-from-pair'([A, _], A).
'second-from-pair'([_, A], A).
'unique-atom'(A, B) :- list_to_set(A, B).
'sort-atom'(List, Sorted) :- msort(List, Sorted).
'size-atom'(List, Size) :- length(List, Size).
'car-atom'([H|_], H).
'cdr-atom'([_|T], T).
decons([H|T], [H|[T]]).
cons(H, T, [H|T]).
'index-atom'(List, Index, Elem) :- nth0(Index, List, Elem).
member(X, L, _) :- member(X, L).
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
get_function_type([F|Args], T) :- nonvar(F), match('&self', [':',F,[->|Ts]], _, _),
                                 append(As,[T],Ts),
                                 maplist('get-type',Args,As).

'get-type'(X, T) :- (get_type_candidate(X, T) *-> true ; T = '%Undefined%' ).

get_type_candidate(X, 'Number')   :- number(X), !.
get_type_candidate(X, '%Undefined%') :- var(X), !.
get_type_candidate(X, 'String')   :- string(X), !.
get_type_candidate(true, 'Bool')  :- !.
get_type_candidate(false, 'Bool') :- !.
get_type_candidate(X, T) :- get_function_type(X,T).
get_type_candidate(X, T) :- \+ get_function_type(X, _),
                            is_list(X),
                            maplist('get-type', X, T).
get_type_candidate(X, T) :- match('&self', [':',X,T], T, _).

'get-metatype'(X, 'Variable') :- var(X), !.
'get-metatype'(X, 'Grounded') :- number(X), !.
'get-metatype'(X, 'Grounded') :- string(X), !.
'get-metatype'(true,  'Grounded') :- !.
'get-metatype'(false, 'Grounded') :- !.
'get-metatype'(X, 'Grounded') :- atom(X), fun(X), !.  % e.g., '+' is a registered fun/1
'get-metatype'(X, 'Expression') :- is_list(X), !.     % e.g., (+ 1 2), (a b)
'get-metatype'(X, 'Symbol') :- atom(X), !.            % e.g., a

'is-var'(A,R) :- var(A) -> R=true ; R=false.
'is-expr'(A,R) :- is_list(A) -> R=true ; R=false.
'is-space'(A,R) :- atom(A), atom_concat('&', _, A) -> R=true ; R=false.

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
                  format("is ~w, should ~w. ~w ~n", [RA, RB, E]),
                  (A =@= B -> true ; halt(1)).

assert(Goal, true) :- ( call(Goal) -> true
                                    ; swrite(Goal, RG),
                                      format("Assertion failed: ~w~n", [RG]),
                                      halt(1) ).

%%% Time Retrieval: %%%
'current-time'(Time) :- get_time(Time).
'format-time'(Format, TimeString) :- get_time(Time), format_time(atom(TimeString), Format, Time).

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

%%% Higher-Order Functions: %%%
'foldl-atom'([], Acc, _Func, Acc).
'foldl-atom'([H|T], Acc0, Func, Out) :- reduce([Func,Acc0,H], Acc1),
                                        'foldl-atom'(T, Acc1, Func, Out).

'map-atom'([], _Func, []).
'map-atom'([H|T], Func, [R|RT]) :- reduce([Func,H], R),
                                   'map-atom'(T, Func, RT).

'filter-atom'([], _Func, []).
'filter-atom'([H|T], Func, Out) :- ( reduce([Func,H], true) -> Out = [H|RT]
                                                             ; Out = RT ),
                                   'filter-atom'(T, Func, RT).

%%% Prolog interop: %%%
normalize_fun_spec(Spec0, ModuleSpec, Fun) :-
    ( string(Spec0) -> atom_string(Spec, Spec0)
    ; Spec = Spec0 ),
    ( Spec = ModuleAtom:FunAtom
    -> ModuleSpec = ModuleAtom, Fun = FunAtom
    ; Spec = [ModuleAtom, FunAtom]
    -> ModuleSpec = ModuleAtom, Fun = FunAtom
    ; ModuleSpec = auto, Fun = Spec ),
    must_be(atom, Fun).

candidate_modules(auto, Candidates) :-
    setof(M, current_module(M), Modules),
    subtract(Modules, [metta, user, system], Others),
    Candidates = [metta, user, system | Others].
candidate_modules(Module, [Module]).

import_prolog_function(Spec, true) :-
    normalize_fun_spec(Spec, ModuleSpec, Fun),
    candidate_modules(ModuleSpec, Candidates),
    member(Module, Candidates),
    maybe_load_module(Module),
    ( import_from_namespace(Module, Fun) -> true ),
    !,
    register_fun(Fun).
import_prolog_function(Spec, true) :-
    normalize_fun_spec(Spec, _, Fun),
    throw(error(existence_error(procedure, Fun), _)).
'Predicate'([F|Args], Term) :- Term =.. [F|Args].
callPredicate(G, true) :- call_in_namespace(G).
assertzPredicate(G, true) :- assertz(G).
assertaPredicate(G, true) :- asserta(G).
retractPredicate(G, true) :- retract(G), !.
retractPredicate(_, false).

%%% Registration: %%%
'import!'('&self', File, true) :- atom_string(File, SFile),
                                  working_dir(Base),
                                  atomic_list_concat([Base, '/', SFile, '.metta'], Path),
                                  load_metta_file(Path,_).

:- dynamic fun/1.
register_fun(N) :- (fun(N) -> true ; assertz(fun(N))).
unregister_fun(N/Arity) :- retractall(fun(N)),
                           retractall(arity(N, _)),
                           abolish(N, Arity).

ensure_local_arities(F) :-
    forall(current_predicate(metta:F/Arity),
           ensure_fun_arity(F, Arity)).

register_builtin_fun(F) :-
    import_user_fun(F),
    register_fun(F),
    ensure_local_arities(F).

:- maplist(register_builtin_fun, [superpose, empty, let, 'let*', '+','-','*','/', '%', min, max, 'change-state!', 'get-state', 'bind!',
                          '<','>','==', '!=', '=', '=?', '<=', '>=', and, or, xor, not, sqrt, exp, log, cos, sin,
                          'first-from-pair', 'second-from-pair', 'car-atom', 'cdr-atom', 'unique-atom',
                          repr, repra, 'println!', 'readln!', 'trace!', test, assert, 'mm2-exec', atom_concat, atom_chars, copy_term, term_hash,
                          foldl, append, length, 'size-atom', sort, msort, member, 'is-member', 'exclude-item', list_to_set, maplist, eval, reduce, 'import!',
                          'add-atom', 'remove-atom', 'get-atoms', match, 'is-var', 'is-expr', 'is-space', 'get-mettatype',
                          decons, 'decons-atom', 'py-call', 'get-type', 'get-metatype', '=alpha', concat, sread, cons, reverse,
                          '#+','#-','#*','#div','#//','#mod','#min','#max','#<','#>','#=','#\\=',
                          'union-atom', 'cons-atom', 'intersection-atom', 'subtraction-atom', 'index-atom', id, use_module,
                          'pow-math', 'sqrt-math', 'sort-atom','abs-math', 'log-math', 'trunc-math', 'ceil-math',
                          'floor-math', 'round-math', 'sin-math', 'cos-math', 'tan-math', 'asin-math','random-int','random-float',
                          'acos-math', 'atan-math', 'isnan-math', 'isinf-math', 'min-atom', 'max-atom',
                          'foldl-atom', 'map-atom', 'filter-atom','current-time','format-time',
                          import_prolog_function, 'Predicate', callPredicate, assertaPredicate, assertzPredicate, retractPredicate]).

:- maplist(wrap_module_fun(system), [sort/2, msort/2, length/2]).

%Pattern matching, structural and functional/relational constraints on arguments:
constrain_args(X, X, []) :- (var(X); atomic(X)), !.
constrain_args([F, A, B], [A|B], []) :- F == cons, !.
constrain_args([F|Args], Var, Goals) :- atom(F),
                                        fun(F), !,
                                        translate_expr([F|Args], GoalsExpr, Var),
                                        flatten(GoalsExpr, Goals).
constrain_args([F|Args], [F|Args1], Goals) :- maplist(constrain_args, Args, Args1, NestedGoalsList),
                                              flatten(NestedGoalsList, Goals), !.

%Flatten (= Head Body) MeTTa function into Prolog Clause:
translate_clause(Input, (Head :- BodyConj)) :- Input = [=, [F|Args0], BodyExpr],
                                               maplist(constrain_args, Args0, Args1, GoalsA),
                                               append(GoalsA, GoalsPrefix),
                                               append(Args1, [Out], Args),
                                               compound_name_arguments(Head, F, Args),
                                               translate_expr(BodyExpr, GoalsB, Out),
                                               append(GoalsPrefix, GoalsB, Goals),
                                               goals_list_to_conj(Goals, BodyConj).

%Conjunction builder, turning goals list to a flat conjunction:
goals_list_to_conj([], true)      :- !.
goals_list_to_conj([G], G)        :- !.
goals_list_to_conj([G|Gs], (G,R)) :- goals_list_to_conj(Gs, R).

% Runtime dispatcher: call F if it's a registered fun/1, else keep as list:
reduce([F|Args], Out) :- ( nonvar(F), atom(F), fun(F) -> append(Args, [Out], CallArgs),
                                                         Goal =.. [F|CallArgs],
                                                         call(Goal)
                                                       ; Out = [F|Args],
                                                         \+ cyclic_term(Out) ).

%Combined expr translation to goals list
translate_expr_to_conj(Input, Conj, Out) :- translate_expr(Input, Goals, Out),
                                            goals_list_to_conj(Goals, Conj).

%Special stream operation rewrite rules before main translation
rewrite_streamops(['unique', ['superpose'|Args]],
                  ['call', ['superpose', ['unique-atom', ['collapse', ['superpose'|Args]]]]]).
rewrite_streamops(['union', ['superpose'|A], ['superpose'|B]],
                  ['call', ['superpose', ['union-atom', ['collapse', ['superpose'|A]],
                                                        ['collapse', ['superpose'|B]]]]]).
rewrite_streamops(['intersection', ['superpose'|A], ['superpose'|B]],
                  ['call', ['superpose', ['intersection-atom', ['collapse', ['superpose'|A]],
                                                               ['collapse', ['superpose'|B]]]]]).
rewrite_streamops(['subtraction', ['superpose'|A], ['superpose'|B]],
                  ['call', ['superpose', ['subtraction-atom', ['collapse', ['superpose'|A]],
                                                              ['collapse', ['superpose'|B]]]]]).
rewrite_streamops(X, X).

%Guarded stream ops rewrite rule application, successfully avoiding copy_term:
safe_rewrite_streamops(In, Out) :- ( compound(In), In = [Op|_], atom(Op) -> rewrite_streamops(In, Out)
                                                                          ; Out = In).

%Turn MeTTa code S-expression into goals list:
translate_expr(X, [], X)          :- (var(X) ; atomic(X)), !.
translate_expr([H0|T0], Goals, Out) :-
        safe_rewrite_streamops([H0|T0],[H|T]),
        translate_expr(H, GsH, HV),
        %--- Non-determinism ---:
        ( HV == superpose, T = [Args], is_list(Args) -> build_superpose_branches(Args, Out, Branches),
                                                        disj_list(Branches, Disj),
                                                        append(GsH, [Disj], Goals)
        ; HV == collapse, T = [E] -> translate_expr_to_conj(E, Conj, EV),
                                     append(GsH, [findall(EV, Conj, Out)], Goals)
        ; HV == cut, T = [] -> append(GsH, [(!)], Goals),
                               Out = true
        ; HV == once, T = [X] -> translate_expr_to_conj(X, Conj, Out),
                                 append(GsH, [once(Conj)], Goals)
        ; HV == hyperpose, T = [L],
          build_hyperpose_branches(L, Branches),
          append(GsH, [concurrent_and(member((Goal,Res), Branches), (call(Goal), Out = Res))], Goals)
        %--- Conditionals ---:
        ; HV == if, T = [Cond, Then, Else] -> ( translate_expr_to_conj(Cond, ConC, true),
                                                translate_expr_to_conj(Then, ConT, Vt)
                                                -> handle_if_condition(ConC, ConT, Else, GsH, Goals, Vt, Out)
                                                 ; translate_expr_to_conj(Else, ConE, Out),
                                                   ( ConE == true -> GsH = Goals ; append(GsH, [ConE], Goals) ))
        ; HV == case, T = [KeyExpr, PairsExpr] -> translate_expr(KeyExpr, Gk, Kv),
                                                  translate_case(PairsExpr, Kv, Out, IfGoal),
                                                  append(GsH, Gk, G0),
                                                  append(G0, [IfGoal], Goals)
        %--- Unification constructs ---:
        ; HV == let, T = [Pat, Val, In] -> translate_expr(Pat, Gp, Pv),
                                           constrain_args(Pv, P, Gc),
                                           translate_expr(Val, Gv, V),
                                           translate_expr(In,  Gi, Out),
                                           append([GsH,[(P=V)],Gp,Gv,Gi,Gc], Goals)
        ; HV == 'let*', T = [Binds, Body] -> letstar_to_rec_let(Binds,Body,RecLet),
                                             translate_expr(RecLet,  Goals, Out)
        ; HV == chain, T = [Eval, Pat, After] -> translate_pattern(Pat, P),
                                                 translate_expr(Eval, Ge, Ev),
                                                 translate_expr(After, Ga, A),
                                                 Goal = let(P, Ev, A, Out),
                                                 append(GsH, Ge, A0), append(A0, Ga, Inner),
                                                 Goals = [Goal | Inner]
        ; HV == sealed, T = [Vars, Expr] -> translate_expr_to_conj(Expr, Con, Out),
                                    Goals = [copy_term(Vars,Con,_,Ncon),Ncon]
        %--- Iterating over non-deterministic generators without reification ---:
        ; HV == 'forall', T = [[GF], TF] -> GCall =.. [GF, X],
                                            TCall =.. [TF, X, Truth],
                                            U = [( forall(GCall, (TCall, Truth==true)) -> Out=true ; Out=false )],
                                            append(GsH, U, Goals)
        ; HV == 'foldall', T = [AF, [GF], InitS] -> translate_expr_to_conj(InitS, ConjInit, Init),
                                                    Agg   =.. [AF, X],
                                                    GCall =.. [GF, X],
                                                    append(GsH, [ConjInit, foldall(Agg, GCall, Init, Out)], Goals)
        %--- Higher-order functions with pseudo-lambdas ---:
        ; HV == 'foldl-atom', T = [List, Init, AccVar, XVar, Body]
          -> translate_expr_to_conj(List, ConjList, L),
             translate_expr_to_conj(Init, ConjInit, InitV),
             translate_expr_to_conj(Body, BodyConj, BodyGoal),
             exclude(==(true), [ConjList, ConjInit], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [foldl([XVar, AccVar, NewAcc]>>(BodyConj, NewAcc is BodyGoal), L, InitV, Out)], Goals)
        ; HV == 'map-atom', T = [List, XVar, Body]
          -> translate_expr_to_conj(List, ConjList, L),
             translate_expr_to_conj(Body, BodyCallConj, BodyCall),
             exclude(==(true), [ConjList], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [maplist([XVar, Y]>>(BodyCallConj, Y is BodyCall), L, Out)], Goals)
        ; HV == 'filter-atom', T = [List, XVar, Cond]
          -> translate_expr_to_conj(List, ConjList, L),
             translate_expr_to_conj(Cond, CondConj, CondGoal),
             exclude(==(true), [ConjList], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [include([XVar]>>(CondConj, CondGoal), L, Out)], Goals)
        %--- Spaces ---:
        ; ( HV == 'add-atom' ; HV == 'remove-atom' ) -> append(T, [Out], RawArgs),
                                                        Goal =.. [HV|RawArgs],
                                                        append(GsH, [Goal], Goals)
        ; HV == match, T = [Space, Pattern, Body] -> translate_expr(Space, G1, S),
                                                     translate_expr(Body, GsB, Out),
                                                     append(G1, [match(S, Pattern, Out, Out)], G2),
                                                     append(G2, GsB, Goals)
        %--- Manual dispatch options: ---
        %Generate a predicate call on compilation, translating Args for nesting:
        ; HV == call,  T = [Expr] -> Expr = [F|Args],
                                     translate_args(Args, GsArgs, ArgsOut),
                                     append(GsH, GsArgs, Inner),
                                     append(ArgsOut, [Out], CallArgs),
                                     Goal =.. [F|CallArgs],
                                     append(Inner, [Goal], Goals)
        %Produce a dynamic dispatch, translating Args for nesting:
        ; HV == reduce, T = [Expr] -> Expr = [F|Args],
                                      translate_args(Args, GsArgs, ArgsOut),
                                      append(GsH, GsArgs, Inner),
                                      ExprOut = [F|ArgsOut],
                                      Goal = reduce(ExprOut, Out),
                                      append(Inner, [Goal], Goals)
        %Invoke translator to evaluate MeTTa code as data/list:
        ; HV == eval, T = [Arg] -> append(GsH, [], Inner),
                                   Goal = eval(Arg, Out),
                                   append(Inner, [Goal], Goals)
        %Force arg to remain data/list:
        ; HV == quote, T = [Expr] -> append(GsH, [], Inner),
                                     Out = Expr,
                                     Goals = Inner
        ; HV == 'catch', T = [Expr] ->
          translate_expr(Expr, GsExpr, ExprOut),
          append(GsH, [], Inner),
          goals_list_to_conj(GsExpr, Conj),
          Goal = catch((Conj, Out = ExprOut),
                       Exception,
                       (Exception = error(Type, Ctx) -> Out = ['Error', Type, Ctx]
                                                      ; Out = ['Error', Exception])),
          append(Inner, [Goal], Goals)
        %--- Automatic 'smart' dispatch, translator deciding when to create a predicate call, data list, or dynamic dispatch: ---
        ; translate_args(T, GsT, AVs),
          append(GsH, GsT, Inner),
          %Known function => direct call:
          ( atom(HV), fun(HV) % Check for type definition [:,HV,TypeChain]
            -> ( catch(match('&self', [':', HV, TypeChain], TypeChain, TypeChain), _, fail)
                 -> TypeChain = [->|Xs],
                    append(ArgTypes, [OutType], Xs),
                    translate_args_by_type(T, ArgTypes, GsT2, AVs2),
                    append(GsH, GsT2, Inner2),
                    append(AVs2, [Out], ArgsV),
                    Goal =.. [HV|ArgsV],
                    ( OutType == '%Undefined%'
                      -> append(Inner2, [Goal], Goals)
                       ; append(Inner2, [Goal, ('get-type'(Out, OutType) ; 'get-metatype'(Out, OutType))], Goals) )
                  ; append(AVs, [Out], ArgsV),
                    Goal =.. [HV|ArgsV],
                    append(Inner, [Goal], Goals) )
          %Literals (numbers, strings, etc.), known non-function atom => data:
          ; ( atomic(HV), \+ atom(HV) ; atom(HV), \+ fun(HV) ) -> Out = [HV|AVs],
                                                                  Goals = Inner
          %Plain data list: evaluate inner fun-sublists
          ; is_list(HV) -> eval_data_term(HV, Gd, HV1),
                           append(Inner, Gd, Goals),
                           Out = [HV1|AVs]
          %Unknown head (var/compound) => runtime dispatch:
          ; append(Inner, [reduce([HV|AVs], Out)], Goals) )).

%Selectively apply translate_args for non-Expression args while Expression args stay as data input:
translate_args_by_type([], _, [], []) :- !.
translate_args_by_type([A|As], [T|Ts], GsOut, [AV|AVs]) :-
                      ( T == 'Expression' -> AV = A, GsA = []
                                           ; translate_expr(A, GsA1, AV),
                                             ( T == '%Undefined%'
                                               -> GsA = GsA1
                                                ; append(GsA1, [('get-type'(AV, T) ; 'get-metatype'(AV, T))], GsA))),
                                             translate_args_by_type(As, Ts, GsRest, AVs),
                                             append(GsA, GsRest, GsOut).

%Handle data list:
eval_data_term(X, [], X) :- (var(X); atomic(X)), !.
eval_data_term([F|As], Goals, Val) :- ( atom(F), fun(F) -> translate_expr([F|As], Goals, Val)
                                                         ; eval_data_list([F|As], Goals, Val) ).

%Handle data list entry:
eval_data_list([], [], []).
eval_data_list([E|Es], Goals, [V|Vs]) :- ( is_list(E) -> eval_data_term(E, G1, V) ; V = E, G1 = [] ),
                                         eval_data_list(Es, G2, Vs),
                                         append(G1, G2, Goals).


%Convert let* to recusrive let:
letstar_to_rec_let([[Pat,Val]],Body,[let,Pat,Val,Body]).
letstar_to_rec_let([[Pat,Val]|Rest],Body,[let,Pat,Val,Out]) :- letstar_to_rec_let(Rest,Body,Out).

%Patterns: variables, atoms, numbers, lists:
translate_pattern(X, X) :- var(X), !.
translate_pattern(X, X) :- atomic(X), !.
translate_pattern([H|T], [P|Ps]) :- !, translate_pattern(H, P),
                                       translate_pattern(T, Ps).

% Handles cases where condition translation succeeded.
handle_if_condition(true, true,  _, Goals, Goals, Out, Out) :- !.
handle_if_condition(true, ConT,  _, GsH, Goals, Out, Out) :- append(GsH, [ConT], Goals), !.
handle_if_condition(ConC, ConT, Else, GsH, Goals, Vt, Out) :- translate_expr(Else, Ge, Ve),
                                                              goals_list_to_conj(Ge, ConE),
                                                              build_branch(ConT, Vt, Out, Bt),
                                                              build_branch(ConE, Ve, Out, Be),
                                                              append(GsH, [ConC -> Bt ; Be], Goals).

% Constructs the goal for a single branch of an if-then-else/case.
build_branch(true, Val, Out, (Out = Val)) :- !.
build_branch(Con, Val, Out, Goal) :- var(Val) -> Val = Out, Goal = Con
                                               ; Goal = (Val = Out, Con).

%Translate case expression recursively into nested if:
translate_case([[K,VExpr]|Rs], Kv, Out, Goal) :- translate_expr_to_conj(VExpr, ConV, VOut),
                                                 build_branch(ConV,VOut,Out,Then),
                                                 (Rs == [] -> Goal = ((Kv = K) -> Then)
                                                            ; translate_case(Rs, Kv, Out, Next),
                                                              Goal = ((Kv = K) -> Then ; Next)).

%Translate arguments recursively:
translate_args([], [], []).
translate_args([X|Xs], Goals, [V|Vs]) :- translate_expr(X, G1, V),
                                         translate_args(Xs, G2, Vs),
                                         append(G1, G2, Goals).

%Build A ; B ; C ... from a list:
disj_list([G], G).
disj_list([G|Gs], (G ; R)) :- disj_list(Gs, R).

%Build one disjunct per branch: (Conj, Out = Val):
build_superpose_branches([], _, []).
build_superpose_branches([E|Es], Out, [B|Bs]) :- translate_expr_to_conj(E, Conj, Val),
                                                 build_branch(Conj, Val, Out, B),
                                                 build_superpose_branches(Es, Out, Bs).

%Build hyperpose branch as a goal list for concurrent_maplist to consume:
build_hyperpose_branches([], []).
build_hyperpose_branches([E|Es], [(Goal, Res)|Bs]) :- translate_expr_to_conj(E, Goal, Res),
                                                      build_hyperpose_branches(Es, Bs).

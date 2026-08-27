%Pattern matching, structural and functional/relational constraints on arguments:
constrain_args(X, X, []) :- (var(X); atomic(X)), !.
constrain_args([F, A, B], Out, Goals) :- nonvar(F),
                                         F == cons,
                                         constrain_args(A, A1, G1),
                                         constrain_args(B, B1, G2),
                                         Out = [A1|B1],
                                         append(G1, G2, Goals), !.
constrain_args([F|Args], Var, Goals) :- atom(F),
                                        callable_fun(F, _), !,
                                        translate_expr([F|Args], GoalsExpr, Var),
                                        flatten(GoalsExpr, Goals).
constrain_args(In, Out, Goals) :- maplist(constrain_args, In, Out, NestedGoalsList),
                                  flatten(NestedGoalsList, Goals), !.

%Flatten (= Head Body) MeTTa function into Prolog Clause:
translate_clause(Input, (Head :- BodyConj)) :- translate_clause(Input, (Head :- BodyConj), true).
translate_clause(Input, (Head :- BodyConj), ConstrainArgs) :-
                                               Input = [=, [F|Args0], BodyExpr],
                                               current_translation_space(Space),
                                               space_pred(Space, F, Pred),
                                               ( ConstrainArgs -> maplist(constrain_args, Args0, Args1, GoalsA),
                                                                  flatten(GoalsA,GoalsPrefix)
                                                                ; Args1 = Args0, GoalsPrefix = [] ),
                                               catch(nb_getval(Pred, Prev), _, Prev = []),
                                               nb_setval(Pred, [fun_meta(Args1, BodyExpr) | Prev]),
                                               ( declared_output_type(F, 'Atom')
                                                 -> GoalsBody = [],
                                                    ExpOut = BodyExpr
                                                  ; translate_expr(BodyExpr, GoalsBody, ExpOut) ),
                                               (  nonvar(ExpOut) , ExpOut = partial(Base,Bound)
                                               -> arity(Base, Arity), length(Bound, N), M is (Arity - N) - 1,
                                                  length(ExtraArgs, M), append(Bound, ExtraArgs, CallInArgs),
                                                  resolve_memoization(Base, CallInArgs, Out, Goal),
                                                  append(GoalsBody,[Goal],FinalGoals), append(Args1,ExtraArgs,HeadArgs)
                                               ; FinalGoals= GoalsBody , HeadArgs = Args1, Out = ExpOut ),
                                               append(HeadArgs, [Out], FinalArgs),
                                               Head =.. [Pred|FinalArgs],
                                               length(FinalArgs, CompiledArity),
                                               (arity(Pred, CompiledArity) -> true ; assertz(arity(Pred, CompiledArity))),
                                               append(GoalsPrefix, FinalGoals, Goals),
                                               goals_list_to_conj(Goals, BodyConj).

%%% Executable space ownership %%%

:- dynamic space_fun/3.
:- dynamic translated_from/3.

% Existing compiler clients operate on the default program only.
translated_from(Ref, Term) :- translated_from(Ref, '&self', Term).

current_translation_space(Space) :-
    ( nb_current('$translation_space', Current) -> Space = Current
    ; Space = '&self' ).

with_translation_space(Space, Goal) :-
    current_translation_space(Previous),
    ( Previous == Space -> call(Goal)
    ; setup_call_cleanup(nb_setval('$translation_space', Space),
                         Goal,
                         nb_setval('$translation_space', Previous)) ).

translate_expr_in_space(Space, Input, Goals, Out) :-
    with_translation_space(Space, translate_expr(Input, Goals, Out)).

space_pred('&self', F, F) :- !.
space_pred(Space, F, Pred) :-
    atom_length(Space, SpaceLength),
    atom_length(F, FunctionLength),
    atomic_list_concat(['$petta_space$', SpaceLength, '$', Space,
                        '$', FunctionLength, '$', F], Pred).

register_space_fun(Space, F, Pred) :-
    space_pred(Space, F, Pred),
    ( space_fun(Space, F, Pred) -> true
    ; assertz(space_fun(Space, F, Pred)) ),
    register_fun(Pred).

space_call_target(Space, F, Pred) :-
    ( space_fun(Space, F, Owned) -> Pred = Owned
    ; Space == '&self' -> fun(F), Pred = F
    ; fun(F), \+ space_fun('&self', F, _) -> Pred = F
    ).

callable_fun(F, Pred) :-
    current_translation_space(Space),
    space_call_target(Space, F, Pred).

reduce_goal(Term, Out, Goal) :-
    current_translation_space(Space),
    ( Space == '&self' -> Goal = reduce(Term, Out)
    ; Goal = space_reduce(Space, Term, Out) ).

type_guard_goal(Value, Type, Guard) :-
    current_translation_space(Space),
    ( Space == '&self'
      -> Guard = ('get-type'(Value, Type) *-> true ; 'get-metatype'(Value, Type))
    ; Guard = ('space-get-type'(Space, Value, Type) *-> true ; 'get-metatype'(Value, Type)) ).

pred_type_chains(Pred, TypeChains) :-
    ( space_fun(Space, F, Pred) -> function_type_chains(Space, F, TypeChains)
    ; function_type_chains(Pred, TypeChains) ).

install_function_clause(Space, Term, Ref) :-
    with_translation_space(Space, once(translate_clause(Term, Clause))),
    assertz(Clause, Ref),
    assertz(translated_from(Ref, Space, Term)).

uninstall_function_clauses(Space, Term, Refs) :-
    findall(Ref, translated_from(Ref, Space, Term), Refs),
    forall(member(Ref, Refs), erase(Ref)),
    retractall(translated_from(_, Space, Term)).

forget_function_provenance(Pred) :-
    findall(Ref,
            ( translated_from(Ref, Space, [=, [F|_], _]),
              space_pred(Space, F, Pred) ),
            Refs),
    forall(member(Ref, Refs), retractall(translated_from(Ref, _, _))).

%%% Coherent function-type revisions %%%

:- thread_local predeclared_function_type/3.

function_type_annotation_term([':', F, TypeChain], F, TypeChain) :-
    atom(F),
    TypeChain = ['->'|Types],
    Types \= [].

with_predeclared_function_types(Space, Types, Goal) :-
    setup_call_cleanup(
        maplist(assert_predeclared_function_type(Space), Types, Refs),
        Goal,
        maplist(erase_predeclared_function_type, Refs)).

assert_predeclared_function_type(Space, F-TypeChain, Ref) :-
    assertz(predeclared_function_type(Space, F, TypeChain), Ref).

erase_predeclared_function_type(Ref) :- catch(erase(Ref), _, true).

predeclared_function_type_variant(Space, F, TypeChain) :-
    predeclared_function_type(Space, F, Existing),
    TypeChain =@= Existing, !.

function_type_chains(F, TypeChains) :-
    current_translation_space(Space),
    function_type_chains(Space, F, TypeChains).

function_type_chains(Space, F, TypeChains) :-
    findall(TypeChain,
            catch(match(Space, [':', F, TypeChain], TypeChain, TypeChain),
                  _, fail),
            Live),
    findall(TypeChain, predeclared_function_type(Space, F, TypeChain), Predeclared),
    append(Live, Predeclared, All),
    variant_unique(All, TypeChains).

variant_unique(Items, Unique) :- variant_unique(Items, [], Unique).

variant_unique([], _, []).
variant_unique([Item|Items], Seen, Unique) :-
    member_variant(Item, Seen), !,
    variant_unique(Items, Seen, Unique).
variant_unique([Item|Items], Seen, [Item|Unique]) :-
    variant_unique(Items, [Item|Seen], Unique).

member_variant(Item, [Seen|_]) :- Item =@= Seen, !.
member_variant(Item, [_|Seen]) :- member_variant(Item, Seen).

% The compiled clause is the authority for which source definition is live.
function_source_clauses(Space, F, Clauses) :-
    findall(Ref-Term,
            ( translated_from(Ref, Space, Term),
              clause(_, _, Ref),
              Term = [=, [SourceF|_], _],
              SourceF == F,
              \+ ho_specialization(_, F) ),
            Clauses).

source_mentions_atom(Term, Atom) :-
    atom(Term), !,
    Term == Atom.
source_mentions_atom(Term, Atom) :-
    nonvar(Term),
    is_list(Term),
    member(Part, Term),
    source_mentions_atom(Part, Atom).

direct_source_dependents(Space, Callee, Functions) :-
    findall(F,
            ( translated_from(Ref, Space, Term),
              clause(_, _, Ref),
              Term = [=, [F|_], _],
              atom(F),
              \+ ho_specialization(_, F),
              source_mentions_atom(Term, Callee) ),
            Functions0),
    sort(Functions0, Functions).

source_dependent_closure(Space, Callee, Functions) :-
    source_dependent_closure(Space, [Callee], [], [], Functions0),
    sort(Functions0, Functions).

source_dependent_closure(_, [], _, Functions, Functions).
source_dependent_closure(Space, [Current|Pending], Seen, Acc, Functions) :-
    memberchk(Current, Seen), !,
    source_dependent_closure(Space, Pending, Seen, Acc, Functions).
source_dependent_closure(Space, [Current|Pending], Seen, Acc, Functions) :-
    direct_source_dependents(Space, Current, Direct),
    append(Pending, Direct, Next),
    append(Direct, Acc, NextAcc),
    source_dependent_closure(Space, Next, [Current|Seen], NextAcc, Functions).

function_metadata(F, present(Metadata)) :-
    catch(nb_getval(F, Metadata), _, fail), !.
function_metadata(_, absent).

restore_function_metadata(F, present(Metadata)) :- !,
    nb_setval(F, Metadata).
restore_function_metadata(F, absent) :-
    catch(nb_delete(F), _, true).

snapshot_function_metadata([], []).
snapshot_function_metadata([F|Functions], [F-State|States]) :-
    function_metadata(F, State),
    snapshot_function_metadata(Functions, States).

restore_function_metadata_snapshot([]).
restore_function_metadata_snapshot([F-State|States]) :-
    restore_function_metadata(F, State),
    restore_function_metadata_snapshot(States).

type_revision_plan(Space, Callee, Plan, MetadataSnapshot) :-
    source_dependent_closure(Space, Callee, Functions),
    maplist(function_recompile_entry(Space), Functions, Plan),
    ( Plan == []
      -> MetadataSnapshot = []
      ;  maplist(space_pred(Space), Functions, Predicates),
         ( Space == '&self'
           -> findall(Specialization, ho_specialization(_, Specialization), Specs)
           ; Specs = [] ),
         append(Predicates, Specs, Symbols0),
         sort(Symbols0, Symbols),
         snapshot_function_metadata(Symbols, MetadataSnapshot) ).

function_recompile_entry(Space, F, function(Space, F, Pred, Clauses)) :-
    space_pred(Space, F, Pred),
    function_source_clauses(Space, F, Clauses).

erase_compiled_clauses([]).
erase_compiled_clauses([Ref-_|Clauses]) :-
    erase(Ref),
    retractall(translated_from(Ref, _, _)),
    erase_compiled_clauses(Clauses).

compile_source_clauses([], _).
compile_source_clauses([_-Term|Clauses], Space) :-
    copy_term(Term, Fresh),
    install_function_clause(Space, Fresh, _),
    compile_source_clauses(Clauses, Space).

recompile_function(function(Space, _F, Pred, Clauses)) :-
    erase_compiled_clauses(Clauses),
    nb_setval(Pred, []),
    compile_source_clauses(Clauses, Space),
    metta_on_function_changed(Pred).

specialization_stack(Stack) :-
    catch(nb_getval('$spec_stack', Stack), _, Stack = []).

with_specialization_suppressed(Goal) :-
    specialization_stack(Prior),
    findall(F, fun(F), Functions0),
    sort(Functions0, Functions),
    setup_call_cleanup(nb_setval('$spec_stack', Functions),
                       Goal,
                       nb_setval('$spec_stack', Prior)).

invalidate_all_specializations :-
    findall(F, ho_specialization(F, _), Functions0),
    sort(Functions0, Functions),
    forall(member(F, Functions), invalidate_specializations(F)).

apply_type_revision(_, []).
apply_type_revision(Space, Plan) :-
    Plan \= [],
    with_specialization_suppressed(
        ( ( Space == '&self' -> invalidate_all_specializations ; true ),
          maplist(recompile_function, Plan) )).

restore_type_revision_state(state(snapshot(MetadataSnapshot))) :- !,
    restore_function_metadata_snapshot(MetadataSnapshot).
restore_type_revision_state(_).

% Type mutation and dependent recompilation are one database transaction.
% Non-backtrackable compiler metadata is restored explicitly on failure.
revise_function_type(Space, F, Mutation) :-
    with_mutex(petta_type_revision,
        ( State = state(none),
          catch(
              ( transaction(
                    ( function_type_chains(Space, F, Before),
                      call(Mutation),
                      function_type_chains(Space, F, After),
                      ( Before =@= After -> true
                      ; type_revision_plan(Space, F, Plan, MetadataSnapshot),
                        nb_setarg(1, State, snapshot(MetadataSnapshot)),
                        apply_type_revision(Space, Plan) ) ))
                -> true
                ;  restore_type_revision_state(State), fail ),
              Error,
              ( restore_type_revision_state(State),
                throw(Error) )) )).

%Print compiled clause:
maybe_print_compiled_clause(_, _, _) :- silent(true), !.
maybe_print_compiled_clause(Label, FormTerm, Clause) :-
    swrite(FormTerm, FormStr),
    format("\e[33m-->  ~w  -->~n\e[36m~w~n\e[33m--> prolog clause -->~n\e[32m", [Label, FormStr]),
    portray_clause(current_output, Clause),
    format("\e[33m^^^^^^^^^^^^^^^^^^^^^~n\e[0m").

%Conjunction builder, turning goals list to a flat conjunction:
goals_list_to_conj([], true)      :- !.
goals_list_to_conj([G], G)        :- !.
goals_list_to_conj([G|Gs], (G,R)) :- goals_list_to_conj(Gs, R).

resolve_memoization(Fun, Args, Out, Goal) :-
    ( metta_memoized_dispatch_call(Fun, Args, Out, Goal)
    -> true
    ; append(Args, [Out], DirectArgs),
      Goal =.. [Fun|DirectArgs]
    ).
incomplete_application_kind(Fun, Arity, partial) :- ( arity(Fun, KnownArity), KnownArity >= Arity
                                                     ; \+ arity(Fun, _) ), !.
incomplete_application_kind(_, _, overapplied).

throw_function_overapplication(Fun, ActualInputArity) :-
    findall(InputArity, (arity(Fun, Arity), InputArity is Arity - 1), InputArities),
    sort(InputArities, KnownInputArities),
    throw(error(domain_error(function_input_arities(Fun, KnownInputArities), ActualInputArity), none)).

% Runtime dispatcher: call F if it's a registered fun/1, else keep as list:
reduce([F|Args], Out) :- nonvar(F), atom(F), fun(F)
                         -> % --- Case 1: callable predicate ---
                            length(Args, N),
                            Arity is N + 1,
                            ( current_predicate(F/Arity) , \+ (current_op(_, _, F), Arity =< 2)
                              -> resolve_memoization(F, Args, Out, Goal),
                                 catch(call(Goal), _, fail)
                            ; incomplete_application_kind(F, Arity, partial)
                              -> Out = partial(F,Args)
                            ; throw_function_overapplication(F, N) )
                          ; % --- Case 2: partial closure ---
                            compound(F), F = partial(Base, Bound) -> append(Bound, Args, NewArgs),
                                                                     reduce([Base|NewArgs], Out)
                          ; % --- Case 3: leave unevaluated ---
                            Out = [F|Args],
                            \+ cyclic_term(Out).

% Named-space dynamic dispatch receives its owner as a compiled constant.
space_reduce(Space, [F|Args], Out) :-
        nonvar(F), atom(F), space_call_target(Space, F, Pred)
        -> length(Args, N),
           Arity is N + 1,
           ( current_predicate(Pred/Arity), \+ (current_op(_, _, Pred), Arity =< 2)
             -> resolve_memoization(Pred, Args, Out, Goal),
                catch(call(Goal), _, fail)
           ; incomplete_application_kind(Pred, Arity, partial)
             -> Out = partial(Pred, Args)
           ; throw_function_overapplication(Pred, N) )
         ; compound(F), F = partial(Base, Bound)
           -> append(Bound, Args, NewArgs),
              space_reduce(Space, [Base|NewArgs], Out)
         ; Out = [F|Args],
           \+ cyclic_term(Out).

%Calling reduce from aggregate function foldall needs this argument wrapping
agg_reduce(AF, Acc, Val, NewAcc) :- reduce([AF, Acc, Val], NewAcc).
space_agg_reduce(Space, AF, Acc, Val, NewAcc) :-
    space_reduce(Space, [AF, Acc, Val], NewAcc).

%Combined expr translation to goals list
translate_expr_to_conj(Input, Conj, Out) :- translate_expr(Input, Goals, Out),
                                            goals_list_to_conj(Goals, Conj).

%Special stream operation rewrite rules before main translation
rewrite_streamops(['trace!', Arg1, Arg2],
                  [progn, ['println!', Arg1], Arg2]).
rewrite_streamops([unique, Arg],
                  [call, [superpose, ['unique-atom', [collapse, Arg]]]]).
rewrite_streamops(['alpha-unique', Arg],
                  [call, [superpose, ['alpha-unique-atom', [collapse, Arg]]]]).
rewrite_streamops([union, [superpose|A], [superpose|B]],
                  [call, [superpose, ['union-atom', [collapse, [superpose|A]],
                                                    [collapse, [superpose|B]]]]]).
rewrite_streamops([intersection, [superpose|A], [superpose|B]],
                  [call, [superpose, ['intersection-atom', [collapse, [superpose|A]],
                                                           [collapse, [superpose|B]]]]]).
rewrite_streamops([subtraction, [superpose|A], [superpose|B]],
                  [call, [superpose, ['subtraction-atom', [collapse, [superpose|A]],
                                                          [collapse, [superpose|B]]]]]).
rewrite_streamops(X, X).

%Guarded stream ops rewrite rule application, successfully avoiding copy_term:
safe_rewrite_streamops(In, Out) :- ( compound(In), In = [Op|_], atom(Op) -> rewrite_streamops(In, Out)
                                                                          ; Out = In).

%Turn MeTTa code S-expression into goals list:
translate_expr(X, [], X)          :- ((var(X) ; atomic(X)) ; X = partial(_,_)), !.
translate_expr([H0|T0], Goals, Out) :-
        safe_rewrite_streamops([H0|T0],[H|T]),
        translate_expr(H, GsH, HV),
        %--- Translator rules ---:
        ( nonvar(HV), translator_rule(HV) -> ( function_type_chains(HV, [TypeChain|_])
                                               -> TypeChain = [->|Xs],
                                                  append(ArgTypes, [_], Xs),
                                                  translate_args_by_type(T, ArgTypes, GsT, T1)
                                                ; translate_args(T, GsT, T1) ),
                                             append(T1,[Gs],Args),
                                             HookCall =.. [HV|Args],
                                             call(HookCall),
                                             translate_expr(Gs, GsE, Out),
                                             append([GsH,GsT,GsE],Goals)
        %--- Non-determinism ---:
        ; HV == superpose, T = [Args], is_list(Args) -> build_superpose_branches(Args, Out, Branches),
                                                        disj_list(Branches, Disj),
                                                        append(GsH, [Disj], Goals)
        ; HV == collapse, T = [E] -> translate_expr_to_conj(E, Conj, EV),
                                     append(GsH, [findall(EV, Conj, Out)], Goals)
        ; HV == cut, T = [] -> append(GsH, [(!)], Goals),
                               Out = true
        ; HV == test, T = [Expr, Expected] -> translate_expr_to_conj(Expr, Conj, Val),
                                              translate_expr(Expected, GsE, ExpVal),
                                              Goal1 = ( findall(Val, Conj, Results),
                                                        (Results = [Actual] -> true
                                                                             ; Actual = Results ) ),
                                              append(GsH, [Goal1], G1),
                                              append(G1, GsE, G2),
                                              append(G2, [test(Actual, ExpVal, Out)], Goals)
        ; HV == once, T = [X] -> translate_expr_to_conj(X, Conj, Out),
                                 append(GsH, [once(Conj)], Goals)
        ; HV == hyperpose, T = [L]
          -> ( nonvar(L), is_list(L)
               -> build_hyperpose_branches(L, Branches),
                  append(GsH, [concurrent_and(member((Goal,Res), Branches), (call(Goal), Out = Res))], Goals)
               ; translate_expr(L, GsL, LV),
                 append(GsH, GsL, Inner),
                 current_translation_space(HyperposeSpace),
                 ( HyperposeSpace == '&self'
                   -> HyperposeGoal = hyperpose_runtime(LV, Out)
                   ; HyperposeGoal = space_hyperpose_runtime(HyperposeSpace, LV, Out) ),
                 append(Inner, [HyperposeGoal], Goals) )
        ; HV == with_mutex, T = [M,X] -> translate_expr_to_conj(X, Conj, Out),
                                         append(GsH, [with_mutex(M,Conj)], Goals)
        ; HV == transaction, T = [X] -> translate_expr_to_conj(X, Conj, Out),
                                        append(GsH, [transaction(Conj)], Goals)
        %--- Sequential execution ---:
        ; HV == progn, T = Exprs -> translate_args(Exprs, GsList, Outs),
                                    append(GsH, GsList, Tmp),
                                    last(Outs, Out),
                                    Goals = Tmp
        ; HV == prog1, T = Exprs -> Exprs = [First|Rest],
                                    translate_expr(First, GsF, Out),
                                    translate_args(Rest, GsRest, _),
                                    append(GsH, GsF, Tmp1),
                                    append(Tmp1, GsRest, Goals)
        %--- Conditionals ---:
        ; HV == if, T = [Cond, Then] -> translate_expr_to_conj(Cond, ConC, Cv),
                                        translate_expr_to_conj(Then, ConT, Tv),
                                        build_branch(ConT, Tv, Out, BT),
                                        ( ConC == true -> append(GsH, [ ( Cv == true -> BT ) ], Goals)
                                                        ; append(GsH, [ ( ConC, ( Cv == true -> BT ) ) ], Goals) )
        ; HV == if, T = [Cond, Then, Else] -> translate_expr_to_conj(Cond, ConC, Cv),
                                              translate_expr_to_conj(Then, ConT, Tv),
                                              translate_expr_to_conj(Else, ConE, Ev),
                                              build_branch(ConT, Tv, Out, BT),
                                              build_branch(ConE, Ev, Out, BE),
                                              ( ConC == true -> append(GsH, [ (Cv == true -> BT ; BE) ], Goals)
                                                              ; append(GsH, [ (ConC, (Cv == true -> BT ; BE)) ], Goals) )
        ; HV == case, T = [KeyExpr, PairsExpr] -> ( select(Found0, PairsExpr, Rest0),
                                                    subsumes_term(['Empty', _], Found0),
                                                    Found0 = ['Empty', DefaultExpr],
                                                    NormalCases = Rest0
                                                    -> translate_expr_to_conj(KeyExpr, GkConj, Kv),
                                                       translate_case(NormalCases, Kv, Out, CaseGoal, KeyGoal),
                                                       translate_expr_to_conj(DefaultExpr, ConD, DOut),
                                                       build_branch(ConD, DOut, Out, DefaultThen),
                                                       Combined = ( (GkConj, CaseGoal) ;
                                                                    \+ GkConj, DefaultThen ),
                                                       append([GsH, KeyGoal, [Combined]], Goals)
                                                     ; translate_expr(KeyExpr, Gk, Kv),
                                                       translate_case(PairsExpr, Kv, Out, IfGoal, KeyGoal),
                                                       append([GsH, Gk, KeyGoal, [IfGoal]], Goals) )
        %--- Short-circuit boolean operators ---:
        ; HV == 'and-then', T = [A, B] -> translate_expr_to_conj(A, ConjA, Av),
                                           translate_expr_to_conj(B, ConjB, Bv),
                                           append(GsH, [(ConjA, (Av == true -> (ConjB, Out = Bv) ; Out = false))], Goals)
        ; HV == 'or-else', T = [A, B] -> translate_expr_to_conj(A, ConjA, Av),
                                          translate_expr_to_conj(B, ConjB, Bv),
                                          append(GsH, [(ConjA, (Av == true -> Out = true ; (ConjB, Out = Bv)))], Goals)
        %--- Unification constructs ---:
        ; (HV == let ; HV == chain), T = [Pat, Val, In] -> translate_expr(Pat, Gp, Pv),
                                                           translate_expr(Val, Gv, V),
                                                           translate_expr(In,  Gi, Out),
                                                           append([GsH,[(Pv=V)],Gp,Gv,Gi], Goals)
        ; HV == 'let*', T = [Binds, Body] -> letstar_to_rec_let(Binds,Body,RecLet),
                                             translate_expr(RecLet,  Goals, Out)
        ; HV == sealed, T = [Vars, Expr] -> translate_expr_to_conj(Expr, Con, Val),
                                            Goals = [copy_term(Vars,[Con,Val],_,[Ncon,Out]),Ncon]
        %--- Iterating over non-deterministic generators without reification ---:
        ; HV == 'forall', T = [GF, TF]
          -> ( is_list(GF) -> GF = [GFH|GFA],
                              translate_expr(GFH, GsGFH, GFHV),
                              translate_args(GFA, GsGFA, GFAv),
                              append(GsGFH, GsGFA, GsGF),
                              GenList = [GFHV|GFAv]
                            ; translate_expr(GF, GsGF, GFHV),
                              GenList = [GFHV] ),
             translate_expr(TF, GsTF, TFHV),
             TestList = [TFHV, V],
             goals_list_to_conj(GsGF, GPre),
             reduce_goal(GenList, V, GenReduce),
             GenGoal = (GPre, GenReduce),
             reduce_goal(TestList, Truth, TestReduce),
             append(GsH, GsTF, Tmp0),
             append(Tmp0, [( forall(GenGoal, ( TestReduce, Truth == true )) -> Out = true ; Out = false )], Goals)
        ; HV == 'foldall', T = [AF, GF, InitS]
          -> translate_expr_to_conj(InitS, ConjInit, Init),
             translate_expr(AF, GsAF, AFV),
             ( GF = [M|_], (M==match ; M==let ; M=='let*') -> LambdaGF = ['|->', [], GF],
                                                              translate_expr(LambdaGF, GsGF, GFHV),
                                                              GenList = [GFHV]
             ; is_list(GF) -> GF = [GFH|GFA],
                              translate_expr(GFH, GsGFH, GFHV),
                              translate_args(GFA, GsGFA, GFAv),
                              append(GsGFH, GsGFA, GsGF),
                              GenList = [GFHV|GFAv]
                            ; translate_expr(GF, GsGF, GFHV),
                              GenList = [GFHV] ),
             append(GsH, GsAF, Tmp1),
             append(Tmp1, GsGF, Tmp2),
             reduce_goal(GenList, V, FoldReduce),
             current_translation_space(FoldSpace),
             ( FoldSpace == '&self' -> AggGoal = agg_reduce(AFV, V)
             ; AggGoal = space_agg_reduce(FoldSpace, AFV, V) ),
             append(Tmp2, [ConjInit, foldall(AggGoal, FoldReduce, Init, Out)], Goals)
        %--- Higher-order functions with pseudo-lambdas and lambdas ---:
        ; HV == 'foldl-atom', T = [List, Init, AccVar, XVar, Body]
          -> translate_expr_to_conj(List, ConjList, L),
             translate_expr_to_conj(Init, ConjInit, InitV),
             translate_expr_to_conj(Body, BodyConj, BG),
             exclude(==(true), [ConjList, ConjInit], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [foldl([XVar, AccVar, NewAcc]>>(BodyConj, ( number(BG) -> NewAcc is BG ; NewAcc = BG )), L, InitV, Out)], Goals)
        ; HV == 'map-atom', T = [List, XVar, Body]
          -> translate_expr_to_conj(List, ConjList, L),
             translate_expr_to_conj(Body, BodyCallConj, BodyCall),
             exclude(==(true), [ConjList], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [maplist([XVar, Y]>>(BodyCallConj, ( number(BodyCall) -> Y is BodyCall ; Y = BodyCall )), L, Out)], Goals)
        ; HV == 'filter-atom', T = [List, XVar, Cond]
          -> translate_expr_to_conj(List, ConjList, L),
             translate_expr_to_conj(Cond, CondConj, CondGoal),
             exclude(==(true), [ConjList], CleanConjs),
             append(GsH, CleanConjs, GsMid),
             append(GsMid, [include([XVar]>>(CondConj, CondGoal), L, Out)], Goals)
        ; HV == '|->', T = [Args, Body] -> next_lambda_name(F),
                                           % find free (non-argument) variables in Body
                                           term_variables(Body, AllVars),
                                           term_variables(Args, ArgVars),
                                           exclude({ArgVars}/[V]>>memberchk_eq(V, ArgVars), AllVars, FreeVars),
                                           append(FreeVars, Args, FullArgs),
                                           % compile clause with all bound + free vars
                                           translate_clause([=, [F|FullArgs], Body], Clause),
                                           current_translation_space(LambdaSpace),
                                           space_pred(LambdaSpace, F, LambdaPred),
                                           register_fun(LambdaPred),
                                           assertz(Clause),
                                           format(atom(Label), "metta lambda (~w)", [LambdaPred]),
                                           maybe_print_compiled_clause(Label, ['|->', Args, Body], Clause),
                                           length(FullArgs, N),
                                           Arity is N + 1,
                                           (arity(LambdaPred, Arity) -> true ; assertz(arity(LambdaPred, Arity))),
                                           % emit closure capturing the environment (free vars)
                                           ( FreeVars == [] -> Out = LambdaPred
                                                             ; Out = partial(LambdaPred, FreeVars) )
        %--- Spaces ---:
        ; ( HV == 'add-atom' ; HV == 'remove-atom' ), T = [Space,Atom] ->
                                                                   translate_expr(Space, G1, S),
                                                                   Goal =.. [HV,S,Atom,Out],
                                                                   append([GsH,G1,[Goal]], Goals)
        ; HV == match, T = [Space, Pattern, Body] -> translate_expr(Space, G1, S),
                                                     translate_expr(Body, GsB, Out),
                                                     append(G1, [match(S, Pattern, Out, Out)], G2),
                                                     append(G2, GsB, Goals)
        %--- Predicate to compiled goal ---:
        ; HV == translatePredicate, T = [Expr] -> Expr = [S|Args],
                                                  translate_args(Args, GsArgs, ArgsOut),
                                                  Goal =.. [S|ArgsOut],
                                                  append(GsH, GsArgs, Inner),
                                                  append(Inner, [Goal], Goals)
        %--- Manual dispatch options: ---
        %Generate a predicate call on compilation, translating Args for nesting:
        ; HV == call,  T = [Expr] -> Expr = [F|Args],
                                     translate_args(Args, GsArgs, ArgsOut),
                                     append(GsH, GsArgs, Inner),
                                     append(ArgsOut, [Out], CallArgs),
                                     Goal =.. [F|CallArgs],
                                     append(Inner, [Goal], Goals)
        %Produce a dynamic dispatch, translating Args for nesting:
        ; HV == reduce, T = [Expr] -> ( var(Expr) -> translate_expr(Expr, GsH, ExprOut),
                                                     reduce_goal(ExprOut, Out, ReduceGoal),
                                                     Goals = [ReduceGoal|GsH]
                                                   ; Expr = [F|Args],
                                                     translate_args(Args, GsArgs, ArgsOut),
                                                     append(GsH, GsArgs, Inner),
                                                     ExprOut = [F|ArgsOut],
                                                     reduce_goal(ExprOut, Out, ReduceGoal),
                                                     append(Inner, [ReduceGoal], Goals) )
        %Invoke translator to evaluate MeTTa code as data/list:
        ; HV == eval, T = [Arg] -> append(GsH, [], Inner),
                                   current_translation_space(EvalSpace),
                                   ( EvalSpace == '&self' -> Goal = eval(Arg, Out)
                                   ; Goal = eval(Arg, EvalSpace, Out) ),
                                   append(Inner, [Goal], Goals)
        %Evaluate within an explicitly selected space:
        ; HV == eval, T = [Arg, SpaceExpr] -> translate_expr(SpaceExpr, GsS, Space),
                                              append(GsH, GsS, Inner),
                                              append(Inner, [eval(Arg, Space, Out)], Goals)
        %A named-space type query stays with its compiled owner:
        ; HV == 'get-type', T = [Arg],
          current_translation_space(TypeSpace), TypeSpace \== '&self'
          -> translate_expr(Arg, GsArg, Value),
             append(GsH, GsArg, Inner),
             append(Inner, ['space-get-type'(TypeSpace, Value, Out)], Goals)
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
          ( is_list(AVs),
            ( atom(HV), callable_fun(HV, Fun), AllAVs = AVs, IsPartial = false
            ; compound(HV), HV = partial(Fun, Bound), append(Bound,AVs,AllAVs), IsPartial = true
            ) % Check for type definition [:,HV,TypeChain]
            -> pred_type_chains(Fun, UniqueTypeChains),
               ( UniqueTypeChains \= []
                 -> length(AllAVs, InputArity),
                    Arity is InputArity + 1,
                    ( incomplete_application_kind(Fun, Arity, ApplicationKind), ApplicationKind == overapplied
                      -> append(GsH, [throw_function_overapplication(Fun, InputArity)], Goals)
                       ; maplist({Fun,T,GsH,IsPartial,Bound,Out}/[TypeChain,BranchGoal]>>(
                                 typed_functioncall_branch(Fun, TypeChain, T, GsH, IsPartial, Bound, Out, BranchGoal)), UniqueTypeChains, Branches),
                         disj_list(Branches, Disj),
                         Goals = [Disj] )
              ; build_call_or_partial(Fun, AllAVs, Out, Inner, [], Goals))
          %Literals (numbers, strings, etc.), known non-function atom => data:
          ; ( atomic(HV), \+ atom(HV) ; atom(HV), \+ callable_fun(HV, _) ) -> Out = [HV|AVs],
                                                                                       Goals = Inner
          %Plain data list: evaluate inner fun-sublists
          ; is_list(HV) -> eval_data_term(HV, Gd, HV1),
                           append(Inner, Gd, Goals),
                           Out = [HV1|AVs]
          %Unknown head (var/compound) => runtime dispatch:
          ; reduce_goal([HV|AVs], Out, ReduceGoal),
            append(Inner, [ReduceGoal], Goals) )).

%Generate actual function call or partial if arity not complete:
build_call_or_partial(Fun, AVs, Out, Inner, Extra, Goals) :- length(AVs, N),
                                                             Arity is N + 1,
                                                             ( maybe_specialize_call(Fun, AVs, Out, Goal)
                                                               -> append(Inner, [Goal|Extra], Goals)
                                                                ; arity(Fun, Arity)
                                                                  -> resolve_memoization(Fun, AVs, Out, Goal),
                                                                     append(Inner, [Goal|Extra], Goals)
                                                                ; incomplete_application_kind(Fun, Arity, partial)
                                                                  -> Out = partial(Fun, AVs),
                                                                     append(Inner, Extra, Goals)
                                                                   ; append(Inner, [throw_function_overapplication(Fun, N)], Goals) ).

%Type function call generation, returns function call plus typechecks for input and output:
typed_functioncall_branch(Fun, TypeChain, T, GsH, IsPartial, Bound, Out, BranchGoal) :-
    TypeChain = [->|Xs],
    append(ArgTypes, [OutType], Xs),
    translate_args_by_type(T, ArgTypes, GsT2, AVsTmp0),
    ( IsPartial -> append(Bound, AVsTmp0, AVsTmp) ; AVsTmp = AVsTmp0 ),
    append(GsH, GsT2, InnerTmp),
    ( (OutType == '%Undefined%' ; OutType == '_' ; OutType == 'Atom')
       -> Extra = []
       ; type_guard_goal(Out, OutType, OutGuard), Extra = [OutGuard] ),
    build_call_or_partial(Fun, AVsTmp, Out, InnerTmp, Extra, GoalsList),
    goals_list_to_conj(GoalsList, BranchGoal).


%Selectively apply translate_args for non-Expression args while Expression args stay as data input:
translate_args_by_type([], _, [], []) :- !.
translate_args_by_type([A|As], [T|Ts], GsOut, [AV|AVs]) :-
                      ( T == 'Atom' -> AV = A, GsA = []
                                           ; translate_expr(A, GsA1, AV),
                                             ( (T == '%Undefined%' ; T == '_')
                                               -> GsA = GsA1
                                                ; type_guard_goal(AV, T, ArgGuard),
                                                  append(GsA1, [ArgGuard], GsA))),
                                             translate_args_by_type(As, Ts, GsRest, AVs),
                                             append(GsA, GsRest, GsOut).

%Handle data list:
eval_data_term(X, [], X) :- (var(X); atomic(X)), !.
eval_data_term([F|As], Goals, Val) :- ( atom(F), callable_fun(F, _) -> translate_expr([F|As], Goals, Val)
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

% Constructs the goal for a single branch of an if-then-else/case.
build_branch(true, Val, Out, (Out = Val)) :- !.
build_branch(Con, Val, Out, Goal) :- var(Val) -> Val = Out, Goal = Con
                                               ; Goal = (Val = Out, Con).

%Translate case expression recursively into nested if:
translate_case([[K,VExpr]|Rs], Kv, Out, Goal, KGo) :- translate_expr_to_conj(VExpr, ConV, VOut),
                                                      constrain_args(K, Kc, Gc),
                                                      build_branch(ConV, VOut, Out, Then),
                                                      ( Rs == [] -> Goal = ((Kv = Kc) -> Then), KGi=[]
                                                                  ; translate_case(Rs, Kv, Out, Next, KGi),
                                                                    Goal = ((Kv = Kc) -> Then ; Next) ),
                                                      append([Gc,KGi], KGo).

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

%Runtime hyperpose path for variable/computed list arguments.
space_hyperpose_runtime(Space, Exprs, Out) :- is_list(Exprs),
                                              concurrent_and(member(Expr, Exprs),
                                                             eval(Expr, Space, Out)).

hyperpose_runtime(Exprs, Out) :- is_list(Exprs),
                                 concurrent_and(member(Expr, Exprs), eval(Expr, Out)).

%Like membercheck but with direct equality rather than unification
memberchk_eq(V, [H|_]) :- V == H, !.
memberchk_eq(V, [_|T]) :- memberchk_eq(V, T).

%Generate readable lambda name:
next_lambda_name(Name) :- ( catch(nb_getval(lambda_counter, Prev), _, Prev = 0) ),
                          N is Prev + 1,
                          nb_setval(lambda_counter, N),
                          format(atom(Name), 'lambda_~d', [N]).

declared_output_type(F, OutType) :- atom(F),
									nonvar(OutType),
									function_type_chains(F, TypeChains),
									member(TypeChain, TypeChains),
									TypeChain = [->|Types],
									append(_, [DeclaredOutType], Types),
									DeclaredOutType == OutType.

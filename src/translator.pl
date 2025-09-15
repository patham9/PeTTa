%Flatten (= Head Body) MeTTa function into Prolog Clause:
translate_clause(Input, (Head :- (BodyConj, Out=OutBody))) :- Input = [=, [F|Args0], BodyExpr],
                                                              append(Args0, [Out], Args),
                                                              compound_name_arguments(Head, F, Args),
                                                              translate_expr(BodyExpr, GoalsB, OutBody),
                                                              goals_list_to_conj(GoalsB, BodyConj).

%Conjunction builder, turning goals list to a flat conjunction:
goals_list_to_conj([], true)      :- !.
goals_list_to_conj([G], G)        :- !.
goals_list_to_conj([G|Gs], (G,R)) :- goals_list_to_conj(Gs, R).

%Extract arguments or superpose arguments as list:
arg_to_list([superpose|T], T) :- !.
arg_to_list(A, [A]).

maybe_call(F, Args, Out) :-
    nonvar(F), atom(F), fun(F) ->
        append(Args,[Out],CallArgs), Goal=..[F|CallArgs], call(Goal)
    ;
    ( F = ['py-atom', Spec] ->
        resolve_base_last_(Spec, Base, Name),
        build_pycall_term_(Base, Name, Args, Term),     % Base:Name(Args...)
        py_call(Term, Out, [])

    ; F = ['py-dot', Obj, Attr] ->
        to_atomish_(Attr, A),
        ( Args == [] ->
            % ---- ZERO-ARG: execute getattr(obj, name)() via eval with locals ----
            py_call(builtins:dict([]), G, []),                                  % {}
            py_call(builtins:dict([['obj', Obj], ['name', A]]), L, []),         % {'obj': Obj, 'name': A}
            py_call(builtins:eval('getattr(obj, name)()', G, L), Out, [])       % -> Out is the return value
          ; % ---- NON-ZERO-ARG: Obj:Attr(Args...) ----
            build_pycall_term_(Obj, A, Args, Term),
            py_call(Term, Out, [])
        )
    ; Out = [F|Args]
    ).

% ---------- internals ----------
build_pycall_term_(Base, Name, Args, Term) :-
    Call =.. [Name|Args], Term =.. [(:), Base, Call].

resolve_base_last_(Spec, Base, Name) :-
    ( string(Spec)->atom_string(A,Spec) ; A=Spec ),
    atomic_list_concat(Parts,'.',A),
    Parts=[Root|Rest], Rest \= [],
    py_call(builtins:'__import__'(Root), M0, []),
    split_last_(Rest, Pref, Name),
    walk_attrs_(M0, Pref, Base).

split_last_([X], [], X).
split_last_([H|T], [H|P], Last) :- split_last_(T, P, Last).

walk_attrs_(Obj0, [], Obj0).
walk_attrs_(Obj0, [Attr|Rest], Obj) :-
    to_atomish_(Attr, A),
    py_call(builtins:getattr(Obj0, A), Next, []),
    walk_attrs_(Next, Rest, Obj).

to_atomish_(X, A) :- ( atom(X)->A=X ; string(X)->atom_string(A,X) ).


%Turn MeTTa code S-expression into goals list:
translate_expr(X, [], X)          :- (var(X) ; atomic(X)), !.
translate_expr([H|T], Goals, Out) :-
        !, translate_expr(H, GsH, HV),
        ( HV == superpose, T = [Args], is_list(Args) -> build_superpose_branches(Args, Out, Branches),
                                                        disj_list(Branches, Disj),
                                                        append(GsH, [Disj], Goals)
        ; HV == collapse, T = [E] -> translate_expr(E, GsE, EV),
                                     goals_list_to_conj(GsE, Conj),
                                     append(GsH, [findall(EV, Conj, Out)], Goals)
        ; HV == if, T = [C, T1, E1] -> translate_expr(C, Gc, Cv), goals_list_to_conj(Gc, ConC),
                                       translate_expr(T1, Gt, Tv), goals_list_to_conj(Gt, ConT),
                                       translate_expr(E1, Ge, Ev), goals_list_to_conj(Ge, ConE),
                                       ( ConT == true -> BT = (Out = Tv) ; BT = (ConT, Out = Tv) ),
                                       ( ConE == true -> BE = (Out = Ev) ; BE = (ConE, Out = Ev) ),
                                       ( ConC == true -> append(GsH, [ (Cv == true -> BT ; BE) ], Goals)
                                                       ; append(GsH, [ (ConC, (Cv == true -> BT ; BE)) ], Goals))
        ; HV == case, T = [KeyExpr, PairsExpr] -> translate_expr(KeyExpr, Gk, Kv),
                                                  translate_case(PairsExpr, Kv, Out, IfGoal),
                                                  append(GsH, Gk, G0),
                                                  append(G0, [IfGoal], Goals)
        ; HV == let, T = [Pat, Val, In] -> translate_expr(Pat, Gp, P),
                                           translate_expr(Val, Gv, V),
                                           translate_expr(In,  Gi, I),
                                           Goal = let(P, V, I, Out),
                                           append(GsH, Gp, A), append(A, Gv, B), append(B, Gi, Inner),
                                           Goals = [Goal | Inner]
        ; HV == 'let*', T = [Binds, Body] -> translate_bindings(Binds, Gb, Bs),
                                             translate_expr(Body,  Gd, B),
                                             Goal = 'let*'(Bs, B, Out),
                                             append(GsH, Gb, A), append(A, Gd, Inner),
                                             Goals = [Goal | Inner]
        ; HV == cut, T = [] -> append(GsH, [(!)], Goals),
                               Out = true
        ; translate_args(T, GsT, AVs),
          append(GsH, GsT, Inner),
          ( atom(HV), fun(HV) -> Out = V,                        %Known function => direct call
                                 append(AVs, [V], ArgsV),
                                 Goal =.. [HV|ArgsV],
                                 append(Inner, [Goal], Goals)
          ; atomic(HV), \+ atom(HV) -> Out = [HV|AVs],           %Literals (numbers, strings, etc.) => data, no dispatcher
                                       Goals = Inner
          ; atom(HV), \+ fun(HV) -> Out = [HV|AVs],              %Known non-function atom => data
                                    Goals = Inner
          ; is_list(HV), HV = [Hdr|Rest], is_list(Hdr), Hdr = [Fh|_], atom(Fh), fun(Fh)
            -> translate_expr(Hdr, Gh, DynHead),                 %First element is a sublist whose head is a known fun
               eval_data_list(Rest, Gr, Rest1),
               append(Inner, Gh, A1), append(A1, Gr, A2),
               Out = V, append(Rest1, AVs, Args0),
               ( atom(DynHead), fun(DynHead) -> append(Args0, [V], ArgsV),
                                                Goal =.. [DynHead|ArgsV],
                                                append(A2, [Goal], Goals)
                                              ; append(A2, [maybe_call(DynHead, Args0, V)], Goals) )
          ; is_list(HV) -> eval_data_term(HV, Gd, HV1),          %Plain data list: evaluate inner fun-sublists
                           append(Inner, Gd, Goals),
                           Out = [HV1|AVs]
          ; append(Inner, [maybe_call(HV, AVs, Out)], Goals) )). %Unknown head (var/compound) => runtime dispatch

%Handle data list:
eval_data_term(X, [], X) :- (var(X); atomic(X)), !.
eval_data_term([F|As], Goals, Val) :- ( atom(F), fun(F) -> translate_expr([F|As], Goals, Val)
                                                         ; eval_data_list([F|As], Goals, Val) ).

%Handle data list entry:
eval_data_list([], [], []).
eval_data_list([E|Es], Goals, [V|Vs]) :- ( is_list(E) -> eval_data_term(E, G1, V) ; V = E, G1 = [] ),
                                         eval_data_list(Es, G2, Vs),
                                         append(G1, G2, Goals).

%Translate bindings without invoking call:
translate_bindings([], [], []).
translate_bindings([[Pat, Val]|Rest], Goals, [[P,V]|Bs]) :- translate_pattern(Pat, P),  %Handle LHS as pure data
                                                            translate_expr(Val, Gv, V), %RHS as normal expr
                                                            translate_bindings(Rest, Gr, Bs),
                                                            append(Gv, Gr, Goals).

%Patterns: variables, atoms, numbers, lists:
translate_pattern(X, X) :- var(X), !.
translate_pattern(X, X) :- atomic(X), !.
translate_pattern([H|T], [P|Ps]) :- !, translate_pattern(H, P),
                                       translate_pattern(T, Ps).


%Translate case expression recursively into nested if:
translate_case([[K,VExpr]|Rs], Kv, Out, Goal) :- translate_expr(VExpr, Gv, VOut),
                                                 goals_list_to_conj(Gv, ConV),
                                                 Test = (Kv = K),
                                                 ( (ConV == true -> Then = (Out = VOut)
                                                                  ; Then = (ConV, Out = VOut)),
                                                   (Rs == []     -> Goal = (Test -> Then)
                                                                  ; translate_case(Rs, Kv, Out, Next),
                                                                    Goal = (Test -> Then ; Next))).

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
build_superpose_branches([E|Es], Out, [B|Bs]) :- translate_expr(E, Gs, Val),
                                                 goals_list_to_conj(Gs, Conj),
                                                 ( Conj == true -> B = (Out = Val)
                                                                 ; B = (Conj, Out = Val) ),
                                                 build_superpose_branches(Es, Out, Bs).

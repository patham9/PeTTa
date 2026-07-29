%%% Constructor-set snapshots %%%
%%%
%%% Two static verdicts are read off the constructor set of a nominal type:
%%% union_member_excluded/3 (this member cannot build an N-element value) and
%%% domain_keys/3 (these are all the value shapes of this type, used by the
%%% exhaustiveness checks). PeTTa's types are OPEN - a constructor may be
%%% declared in any later file - so both verdicts are snapshots, and a
%%% constructor arriving afterwards silently invalidates them.
%%%
%%% Every snapshot is therefore recorded with the key set it saw, and every
%%% new type declaration re-reads them. A set that changed means some clause
%%% was compiled on a premise that no longer holds:
%%%
%%%   - a clause verdict (union narrowing, case coverage) is REDONE - the
%%%     clause goes back through translate_clause/2 with the new constructor
%%%     set, exactly as if the constructor had been declared first. It shares
%%%     recompile_function_clauses/1 with the late-declaration fix, which is
%%%     the same problem in a different disguise.
%%%   - an exhaustiveness verdict has no clause to redo (it is a property of
%%%     the whole clause set, judged in a per-file prepass), so it is simply
%%%     re-run and throws if the function is no longer exhaustive.
%%%
%%% Both are gated behind "nothing was recorded", so a program that never
%%% narrows a union and declares no -[det]-> pays nothing.
:- dynamic ctor_snapshot_use/3.        % ctor_snapshot_use(Type, KeySnapshot, Function)
:- dynamic det_exhaustive_verdict/8.   % det_exhaustive_verdict(F, N, Heads, Consts, Types, File, Line, FormStr)

%Constructor reads are ordinary proof dependencies.  The enclosing clause or
%exhaustiveness boundary decides how to publish them; the prover itself only
%returns the observation.
note_ctor_snapshot(T) :- analysis_emit(dependency(ctor_set(T))).

%Opens the accumulator for one clause translation and records what it read.
%Nests safely (the specializer re-enters the translator) and leaves the outer
%accumulator exactly as it found it, error or not:
with_ctor_snapshot(F, Goal) :-
    analysis_collect(Goal, Events),
    analysis_event_ctor_types(Events, EventTypes),
    analysis_function_decl_dependencies(F, DeclDependencies),
    findall(T, member(ctor_set(T), DeclDependencies), DeclTypes),
    append(EventTypes, DeclTypes, Types0),
    sort(Types0, Types),
    record_ctor_snapshots(F, Types),
    analysis_reemit_non_ctor_events(Events).

%Same accumulator, for a verdict that is NOT a clause: returns the types read
%instead of attributing them to a function (the exhaustiveness prepass).
with_ctor_snapshot_types(Goal, Types) :-
    analysis_collect(Goal, Events),
    analysis_event_ctor_types(Events, Types),
    analysis_reemit_non_ctor_events(Events).

analysis_event_ctor_types(Events, Types) :-
    findall(T, member(dependency(ctor_set(T)), Events), Ts0),
    sort(Ts0, Types).

analysis_reemit_non_ctor_events([]).
analysis_reemit_non_ctor_events([dependency(ctor_set(_))|Events]) :- !,
    analysis_reemit_non_ctor_events(Events).
analysis_reemit_non_ctor_events([Event|Events]) :-
    analysis_emit(Event),
    analysis_reemit_non_ctor_events(Events).

record_ctor_snapshots(F, Ts) :- forall(member(T, Ts),
                                       ( ctor_key_snapshot(T, Keys),
                                         retractall(ctor_snapshot_use(T, _, F)),
                                         assertz(ctor_snapshot_use(T, Keys, F)) )).

%Everything domain_keys/3 and union_member_excluded/3 can see of T: its
%equation-less declared constructors and its declared nullary constants.
ctor_key_snapshot(T, Keys) :- findall(C/K, ( member_ctor(T, K, C)
                                           ; declared_value_type(C, T2), T2 == T, atom(C), \+ fun(C), K = 0 ),
                                      Ks),
                              sort(Ks, Keys).

%Hook on every NEW type declaration (maybe_cache_type_decl/2). Two gates keep
%it off the hot path, cheapest first: a program that has recorded no snapshot
%has nothing to revalidate (which is every program until it narrows a union or
%declares a -[det]->, and in particular the whole builtin-type seeding pass),
%and a declared symbol WITH equations is never a constructor anyway -
%member_ctor/3's own rule, since such a symbol is rewritten at the call site
%and never survives as a value.
note_constructor_set_change(_) :- \+ ctor_snapshot_use(_, _, _),
                                  \+ det_exhaustive_verdict(_, _, _, _, _, _, _, _), !.
note_constructor_set_change(Name) :- fun(Name), !.
note_constructor_set_change(Name) :- revalidate_ctor_snapshots(Name).

%A declaration only ever ADDS to a constructor set, and it adds exactly the
%keys built from the symbol being declared - so only THAT symbol has to be
%tested against each recorded snapshot. Recomputing whole key sets here
%instead would be quadratic in the number of declarations, for no extra
%precision.
new_ctor_key(Name, T, K) :- member_ctor(T, K, Name).
new_ctor_key(Name, T, 0) :- declared_value_type(Name, T2), T2 == T, \+ fun(Name).

revalidate_ctor_snapshots(Name) :-
    findall(F, ( ctor_snapshot_use(T, Keys, F),
                 new_ctor_key(Name, T, K), \+ memberchk(Name/K, Keys) ),
            Fs0),
    sort(Fs0, Fs),
    forall(member(F, Fs),
           ( format(user_error,
                    "Warning: a constructor declared after ~w was compiled changes a type it matched on; recompiling ~w~n",
                    [F, F]),
             recompile_function_clauses(F) )),
    revalidate_det_exhaustiveness(Name).

%Re-run every exhaustiveness verdict whose domain the new symbol enters. A
%verdict that now fails throws det_nonexhaustive/3 from here - reported
%against the clause that made the claim, not against the declaration that
%broke it, because the clause is what has to change (cover the new
%constructor, or say -[semidet]->).
%The location is the one the verdict was made at, in the file that made it -
%not the file being read now, which merely added the constructor:
revalidate_det_exhaustiveness(Name) :-
    forall(( det_exhaustive_verdict(F, N, Heads, Consts, Types, File, Line, Str),
             member(T, Types), once(new_ctor_key(Name, T, _)) ),
           in_metta_file(
               File,
               with_form_location(
                   Line, Str,
                   det_exhaustiveness_proof(Consts, F, N, Heads, _)))).

in_metta_file(File, Goal) :- current_metta_file(Prev),
                             setup_call_cleanup(nb_setval('$metta_file', File),
                                                Goal,
                                                nb_setval('$metta_file', Prev)).

%An earlier branch consumed EVERY (Ctor V1 ... Vk) value: its pattern is
%headed by Ctor at that arity and its arguments are distinct variables, so the
%match cannot fail. A pattern like (CPU foo $a $r) constrains a field and
%consumes nothing.
prior_consumed_ctor(Prior, Ctor, K) :- member(P0, Prior), nonvar(P0), is_list(P0),
                                       P0 = [H|As], H == Ctor,
                                       length(As, K), maplist(var, As),
                                       sort(As, Distinct), length(Distinct, K), !.

%Check the clause body's inferred output type against the declared output type:
clause_output_goals(_, none, _, _, []) :- !.
clause_output_goals(F, out(OT, ATs), ExpOut, BodyExpr, Gs) :-
        %A declared output type variable occurring in no argument type claims
        %parametric polymorphism: only a bottom body (no value of its own) can
        %honor it, so a body with a concrete result type is rejected:
        ( var(OT) -> ( term_variables(ATs, Vs), \+ memberchk_eq(OT, Vs)
                       -> parametric_output_check(F, ExpOut) ; true ),
                     Gs = []
        ; wildcard_type_t(OT) -> Gs = []
        %quoted CODE is exempt from output checking; a quoted literal is just
        %that literal and is checked like any other value:
        ; nonvar(BodyExpr), BodyExpr = [Q, QV], Q == quote, \+ atomic(QV) -> Gs = []
        %EVERY branch feeding the result must fit the declared output type. A
        %branch of undetermined type (the marker) is not one that fits, so it
        %costs a runtime guard - or a rejection under --strict - exactly as an
        %entirely untyped body does. Without this, one typed branch discharged
        %the certification for all of them:
        %An INDEFINITE branch - the unknown marker, or a candidate still an
        %unbound declaration-instance type variable - is not one that fits
        %(see indefinite_candidate/1), so it costs a runtime guard, or a
        %rejection under --strict, exactly as an entirely untyped body does:
        ; var(ExpOut) ->
            ( known_candidates(ExpOut, Cs) ->
                ( member(C, Cs), output_candidate_conflict(C, OT, Bad)
                  -> throw(error(type_conflict(existing(Bad), required(OT)), typecheck))
                ; member(C, Cs), \+ output_candidate_fits(C, OT)
                  -> type_guard(F, ExpOut, OT, Gs)            %possible runtime refinement
                   ; Gs = [] )
            ; type_guard(F, ExpOut, OT, Gs) )
        ; check_value(ExpOut, OT, St),
          ( St == mismatch -> throw(error(literal_type_mismatch(ExpOut, OT), typecheck))
          ; St == unknown -> type_guard(F, ExpOut, OT, Gs)
          ; Gs = [] ) ).

%A merge candidate certifies against the declared output type. A wrapped ground
%literal is discharged by running check_value/3 on the concrete value itself -
%the strongest evidence there is - so (a b) fits (| Number (List Atom)). Every
%other candidate uses the type-level compatibility test, and an INDEFINITE one
%(the unknown marker, or a promised type variable) is never a fit.
output_candidate_fits(C, OT) :- candidate_evidence(C, literal(V)), !, check_value(V, OT, ok).
output_candidate_fits(C, OT) :- \+ indefinite_candidate(C),
                                ( type_compat_soft(C, OT) ; refinement_pair(C, OT) ).

%A DEFINITE contradiction between a merge candidate and the output type: a
%concrete literal check_value/3 rules out, or a non-indefinite type candidate
%with no compatibility or refinement path. Binds Bad to the offending value/type
%for the diagnostic. An indefinite candidate is never a conflict - it costs a
%guard, not a rejection.
output_candidate_conflict(C, OT, V) :- candidate_evidence(C, literal(V)), !, check_value(V, OT, mismatch).
output_candidate_conflict(C, OT, C) :- \+ indefinite_candidate(C),
                                       \+ type_compat_soft(C, OT), \+ refinement_pair(C, OT).

%Under --oracle a statically discharged output certification is re-verified
%at runtime with the checker's OWN value relation (check_value): a definite
%mismatch between the certified type and the actual value throws. The
%reflective guard is deliberately not used here - it is weaker than the
%checker (no constructor typing over erased brands, no quote exemption).
oracle_output_check(DeclOut, Out, Gs0, Gs) :-
    ( oracle_mode(true), DeclOut = out(OT, _), nonvar(OT), \+ wildcard_type_t(OT), Gs0 == []
      -> Gs = [oracle_check(Out, OT)]
       ; Gs = Gs0 ).

%The same treatment for the OTHER half of the certification story: an argument
%obligation that check_call_arg/5 discharged statically (the callee's declared
%parameter type was proved compatible, so no guard was emitted) is re-verified
%at runtime under --oracle. Output certifications alone left every call-site
%obligation unaudited, which is precisely where the constructor-snapshot and
%brand-erasure holes live. A pure addition: no compile-time decision consults
%these goals, they are appended after the branch has already been chosen.
%
%acyclic_term/1: an inferred self-referential type (examples/matespacefast.metta
%builds one) is a rational tree the compiler can hold but assertz/1 cannot store,
%so such a certification is simply not auditable this way and is skipped rather
%than crashing the compile. Reported here so it is not mistaken for a pass.
oracle_arg_check(AV, T, Gs) :- ( oracle_mode(true), nonvar(T), \+ wildcard_type_t(T),
                                 acyclic_term(T), acyclic_term(AV)
                                 -> Gs = [oracle_check(AV, T)]
                                  ; Gs = [] ).

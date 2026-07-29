%%% Whole-clause-set dependency consumers %%%
%
% Clause compilation dependencies are keyed by clause ref in
% dependency_graph.pl.  Exhaustiveness is the one verdict with no individual
% clause to attach to, so its source payload remains here while its dependency
% list is published through record_validation_dependencies/2.
:- dynamic det_exhaustive_verdict/8.

new_ctor_key(Name, T, K) :- member_ctor(T, K, Name).
new_ctor_key(Name, T, 0) :- declared_value_type(Name, T2), T = T2, \+ fun(Name).

revalidate_dependency_consumer(exhaustiveness(F, N), Event) :-
    det_exhaustive_verdict(F, N, StoredHeads, Consts, _, File, Line, Str),
    current_exhaustiveness_heads(F, N, CurrentHeads),
    ( CurrentHeads == [],
      Event = clause_changed(_, runtime)
      -> retractall(det_exhaustive_verdict(F, N, _, _, _, _, _, _)),
         forget_validation_dependencies(exhaustiveness(F, N))
    ; ( CurrentHeads == [] -> Heads = StoredHeads ; Heads = CurrentHeads ),
      in_metta_file(
          File,
          with_form_location(
              Line, Str,
              det_exhaustiveness_proof(Consts, F, N, Heads, Proof))),
      analysis_proof_dependencies(Proof, Dependencies),
      retractall(det_exhaustive_verdict(F, N, _, _, _, _, _, _)),
      assertz(det_exhaustive_verdict(F, N, Heads, Consts, Dependencies,
                                     File, Line, Str)),
      record_validation_dependencies(exhaustiveness(F, N), Dependencies)
    ).

current_exhaustiveness_heads(F, N, Heads) :-
    findall(Args,
            ( translated_from(Ref, [Eq, Head, _]),
              Eq == (=), clause(_, _, Ref),
              nonvar(Head), Head = [F0|Args], F0 == F,
              length(Args, N) ),
            Heads).

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

%%% Clause-level helpers %%%

%Bind declared parameter types onto clause-head variables. For an overloaded
%function, the clause's head patterns filter the declarations: a clause whose
%head selects exactly one overload is checked against it, a clause no overload
%can produce is rejected, and a genuinely ambiguous clause (e.g. all-variable
%head serving every overload) stays unchecked as before.
clause_param_types(F, Args, DeclOut) :- length(Args, N),
                                        findall(ATs-OTx, fn_decl_arity(F, N, ATs, OTx), Decls),
                                        ( Decls == [] -> DeclOut = none
                                        ; Decls = [ATs1-OT] -> maplist(bind_param_type, Args, ATs1), DeclOut = out(OT, ATs1)
                                        ; include(clause_head_survives(Args), Decls, Survivors),
                                          ( Survivors == [] -> throw(error(no_matching_overload(F), typecheck))
                                          ; Survivors = [ATs1-OT] -> maplist(bind_param_type, Args, ATs1),
                                                                     DeclOut = out(OT, ATs1)
                                          ; DeclOut = none ) ).

clause_head_survives(Args, ATs-_) :- \+ \+ maplist(head_arg_soft, Args, ATs).
head_arg_soft(A, T) :- ( var(A) -> true
                       ; check_value(A, T, St) -> St \== mismatch
                       ; true ).

bind_param_type(Arg, T) :- ( nonvar(Arg), Arg = [At, Whole, Inner], At == '@'
                           -> bind_param_type(Whole, T),
                              bind_param_type(Inner, T)
                           ; var(Arg) -> ( nonvar(T) -> ( \+ wildcard_type_t(T) -> add_known_type(Arg, T)
                                                                                  ; true )
                                           %a variable type is the declaration instance: recording it
                                           %lets identical unknowns be recognized (e.g. rcons's $a):
                                           ; add_known_type(Arg, T) )
                           ; list_type(T, ET), Arg = [H|Rest]
                             -> bind_param_type(H, ET),          %type element vars of list patterns
                                bind_param_type(Rest, ['List', ET])
                           ; is_union(T)                        %clause heads narrow union params
                             -> bind_pattern_typed(Arg, T)
                           ; structural_pattern_fields(Arg, T, Fields, FieldTs)
                             -> maplist(bind_param_type, Fields, FieldTs)
                           ; is_list(Arg), is_list(T), same_length(Arg, T),
                             \+ is_arrow_type(T)                 %untagged tuple types: ($v Number)
                             -> maplist(bind_param_type, Arg, T)
                           ; check_value(Arg, T, St),
                             ( St == mismatch -> throw(error(literal_type_mismatch(Arg, T), typecheck))
                                               ; true ) ).

%Which union member does a pattern's shape select?
pattern_selects_member(P, M) :- nonvar(M), nonvar(P),
                                ( list_type(M, _) -> ( P == [] ; P = [C|_], C == cons ; is_list(P) )
                                ; atom(M) -> \+ \+ structural_pattern_fields(P, M, _, _)
                                ; is_list(M), is_list(P)
                                  -> ( tagged_tuple_type(M, Tag, FTs)
                                       -> P = [Tag2|Fs], Tag2 == Tag, same_length(Fs, FTs)
                                        ; same_length(P, M) )
                                ; fail ).

%Tag evidence outranks shape: (box $pair) also parses as a plain list, but a
%head atom that is a declared constructor of (or the tag of) exactly one
%member makes that member the selection - nominal tags are the idiomatic
%union discriminator (see strict_tuple_types.metta):
pattern_selects_member_tagged(P, M) :- nonvar(M), nonvar(P), P = [Tag|Fs], atom(Tag),
                                       ( atom(M) -> \+ \+ structural_pattern_fields(P, M, _, _)
                                       ; tagged_tuple_type(M, Tag2, FTs), Tag2 == Tag,
                                         same_length(Fs, FTs) ).

%A tagged pattern (Tag P1 ... Pn) against either the structural tuple type
%(Tag T1 ... Tn) or a nominal type produced by Tag's constructor declaration:
structural_pattern_fields(Arg, T, Fields, FieldTs) :- is_list(Arg), Arg = [Tag|Fields], atom(Tag), nonvar(T),
                                                      ( tagged_tuple_type(T, Tag2, FieldTs), Tag2 == Tag,
                                                        same_length(Fields, FieldTs) -> true
                                                      ; atom(T), length(Fields, N),
                                                        findall(ATs-OT, fn_decl_arity(Tag, N, ATs, OT), [FieldTs-OT1]),
                                                        type_compat_soft(OT1, T) ).

%Contextual output typing for deliberately-undeclared builtins (one clause per
%builtin; the translator consults this after translating an undeclared call):
untyped_call_out(cons, [H, Tl], Out) :- cons_out_type(H, Tl, Out).
untyped_call_out('cons-atom', [H, Tl], Out) :- cons_out_type(H, Tl, Out).
untyped_call_out('union-atom', [A, B], Out) :- union_atom_out_type(A, B, Out).
untyped_call_out(append, [A, B], Out) :- union_atom_out_type(A, B, Out).
untyped_call_out('subtraction-atom', [A, _], Out) :- first_list_out_type(A, Out).
untyped_call_out(list_to_set, [A], Out) :- first_list_out_type(A, Out).
%The ACCESSORS, which nobody typed: only the constructors above were here, so
%a (List Choice) went in and an untyped value came out. That is what forced a
%runtime guard on every element use and made --strict reject
%(probe (car-atom $xs)) - the loss was at the accessor, not at match or
%collapse. first/2 is a lib_roman pair helper, a different function; only the
%one-argument builtin is typed here.
untyped_call_out('car-atom', [A], Out) :- list_elem_out_type(A, Out).
untyped_call_out(first, [A], Out) :- list_elem_out_type(A, Out).
untyped_call_out(last, [A], Out) :- list_elem_out_type(A, Out).
untyped_call_out('cdr-atom', [A], Out) :- cdr_atom_out_type(A, Out).

%(List T) -> T. The element type is what the accessor projects; whether the
%call SUCCEEDS is a separate question and belongs to the determinism table
%(where car-atom is det because its second clause answers () for anything the
%first does not match, and last is semidet-or-worse for the empty list).
list_elem_out_type(A, Out) :- ( var(Out), list_source_elem(A, T), nonvar(T),
                                \+ wildcard_type_t(T)
                                -> set_out_type(Out, T) ; true ).

%An expression's tail is always a sequence, so cdr-atom keeps the (List ...)
%floor its old declaration gave it, and narrows the element type whenever the
%argument's own type supplies one:
cdr_atom_out_type(A, Out) :- ( var(Out), list_source_elem(A, T), nonvar(T)
                               -> set_out_type(Out, ['List', T])
                                ; set_out_type(Out, ['List', '%Undefined%']) ).

%Element-filtering builtins preserve their first argument's list type; the
%other operand may be any expression:
first_list_out_type(A, Out) :- ( var(Out), list_source_elem(A, T)
                                 -> set_out_type(Out, ['List', T]) ; true ).

%cons stays undeclared (a global (List $a) signature would reject legal
%heterogeneous expressions). When the head provably fits the tail's list type
%the result is that list type; when it provably does NOT - both types known,
%no fit - the result is still a proper list, of the WIDENED element type: the
%union of what the tail holds and what the head is, exactly how collapse
%records disagreeing branches. (cons () (cons (item 1) ())) is a
%(List (| Item (List ...))), which fits a declared (List Atom) - every member
%fits Atom - while against (List Number) the non-fitting member still costs
%the guard, so nothing is discharged that the value cannot honour. A head of
%UNKNOWN type still yields no claim: unknown is not evidence of anything, a
%union member included.
cons_out_type(H, Tl, Out) :- ( var(Out), list_source_elem(Tl, T)
                               -> ( ( wildcard_type_t(T) -> true    %(List %Undefined%): any head fits
                                    ; var(H) -> known_singleton(H, K), type_unify(K, T)
                                              ; check_value(H, T, St), St == ok )
                                    -> set_out_type(Out, ['List', T])
                                  ; nonvar(T), cons_head_type(H, KH)
                                    -> union_widen(T, KH, U),
                                       set_out_type(Out, ['List', U])
                                     ; true )
                                ; true ).

cons_head_type(H, KH) :- ( var(H) -> known_singleton(H, KH0), nonvar(KH0), KH = KH0
                                   ; value_single_type(H, KH) ).

%The union of two element types, flattening existing unions and deduplicating
%by variant so repeated widening stays small:
union_widen(T, KH, U) :- ( is_union(T) -> T = ['|'|Ms] ; Ms = [T] ),
                         ( is_union(KH) -> KH = ['|'|Ks] ; Ks = [KH] ),
                         variant_union(Ks, Ms, U0),
                         ( U0 = [Single] -> U = Single ; U = ['|'|U0] ).

%union-atom likewise stays undeclared, but concatenating two provably
%compatible lists yields that list type:
union_atom_out_type(A, B, Out) :- ( var(Out),
                                    list_source_elem(A, TA),
                                    list_source_elem(B, TB)
                                    -> ( type_unify(TA, TB)
                                         -> set_out_type(Out, ['List', TA])
                                       %incompatible element types widen, as in cons_out_type/3 -
                                       %the concatenation is still a proper list of both:
                                       ; nonvar(TA), nonvar(TB)
                                         -> union_widen(TA, TB, U),
                                            set_out_type(Out, ['List', U])
                                          ; true )
                                     ; true ).

%The element type carried by a list-valued SOURCE expression, the one question
%behind every list-shape output typer above: a cons TAIL, a union/concat OPERAND,
%an accessor's list ARGUMENT all ask it (cons_tail_elem/2 was a second, identical
%copy). A bound variable answers from its known (List T); a literal () carries no
%element constraint (T stays open, so any head fits); a literal list is read
%element-wise (list_elem_type/2). The callers differ only in how they re-wrap the
%answer - the element itself (car-atom), or (List T) again (cdr-atom, concat):
list_source_elem(X, T) :- ( var(X) -> known_singleton(X, K), list_type(K, T)
                          ; X == [] -> true
                          ; list_elem_type(X, T) ).

%Destructuring bindings: type a pattern's variables from the bound value's
%known type, e.g. (let (Stats $sum $sq $n) (make-stats) ...). With no type for
%the value, a pattern headed by a uniquely declared constructor still knows
%what its own fields are - that is the constructor's declaration talking, not
%the scrutinee's:
bind_pattern_from(Pat, Val) :- ( nonvar(Pat)
                                 -> ( ( var(Val) -> known_singleton(Val, KT)
                                                  ; value_single_type(Val, KT) ),
                                      nonvar(KT)                %an open assumption type says nothing yet
                                      -> bind_pattern_typed(Pat, KT)
                                       ; ctor_pattern_field_types(Pat) )
                                  ; true ).

%Tolerant variant used where a non-matching pattern must not fail or throw
%(case branches: a wrong pattern just never matches at runtime):
bind_pattern_typed(P, T) :- bind_pattern_typed(P, T, []).

%bind_pattern_typed(+Pattern, +Type, +PriorPatterns). PriorPatterns are the
%patterns of EARLIER branches of the same case, in source order, and are empty
%for every other caller (clause heads, let destructuring, meta typing) - those
%are not first-match. They are consulted only for the top-level pattern; field
%patterns recurse with [], since what an earlier branch matched at the top says
%nothing about a nested field.
bind_pattern_typed(P, T, Prior) :-
                            ( nonvar(P), P = [At, Whole, Inner], At == '@'
                              -> bind_pattern_typed(Whole, T, Prior),
                                 bind_pattern_typed(Inner, T, Prior)
                            ; var(P) -> ( nonvar(T), \+ wildcard_type_t(T) -> add_known_type(P, T) ; true )
                            ; is_union(T), T = ['|'|Ms]        %a pattern narrows to the member it selects
                              -> ( findall(M, ( member(M, Ms), pattern_selects_member(P, M) ), [M1]),
                                   narrowing_sound(P, Ms, M1, Prior)
                                   -> bind_pattern_typed(P, M1, Prior)
                                 ; findall(M, ( member(M, Ms), pattern_selects_member_tagged(P, M) ), [M2])
                                   -> bind_pattern_typed(P, M2, Prior) ; true )
                            ; list_type(T, ET), P = [C, H, R], C == cons
                              -> bind_pattern_typed(H, ET),    %source-form (cons H R) destructuring
                                 bind_pattern_typed(R, ['List', ET])
                            ; list_type(T, ET), P = [H|Rest]
                              -> bind_pattern_typed(H, ET),
                                 bind_pattern_typed(Rest, ['List', ET])
                            ; structural_pattern_fields(P, T, Fields, FieldTs)
                              -> maplist(bind_pattern_typed, Fields, FieldTs)
                            ; atom(T), declared_newtype(T, R), \+ wildcard_type_t(R)
                              -> bind_pattern_typed(P, R, Prior)
                            ; is_list(P), is_list(T), same_length(P, T),
                              \+ is_arrow_type(T)
                              -> maplist(bind_pattern_typed, P, T)
                            ; true ).

%%% Soundness gate on union narrowing by shape.
%
%A pattern that carries TAG evidence for the member it selected - its head is
%that member's tag, or a declared constructor of it - says something real
%about the value, and narrows as it always has.
%
%Without that evidence (a variable head: ($_type ($_kbid $_ctx $_vars) $prf
%$tv), or an atom head that is nobody's constructor) the pattern selected the
%member purely by element count, which alone is unsound: another member's
%constructor may build a value of exactly that count (a Goal is
%(CPU $f $a $r), also four elements). Narrowing is then admissible only when
%every OTHER member is ruled out - see union_member_excluded/3.
narrowing_sound(P, _, M1, _) :- pattern_selects_member_tagged(P, M1), !.
narrowing_sound(P, Ms, _, Prior) :- is_list(P), length(P, N),
                                    forall( ( member(M, Ms), \+ pattern_selects_member(P, M) ),
                                            union_member_excluded(M, N, Prior) ).

%union_member_excluded(+Member, +N, +PriorPatterns): no value of Member can be
%an N-element expression here. Either
%  (a) by ARITY - Member has no constructor that builds N elements, or
%  (b) because every constructor of Member that does was already consumed by an
%      EARLIER branch of the same case. case is first-match/committed
%      (translate_case compiles to nested if-then-else), so such a value can
%      never reach this branch.
%(a) reads the constructor set as it stands right now, so the verdict is a
%SNAPSHOT. It used to be a standing limitation - a constructor for Member
%declared in a later file invalidated an exclusion already made and nothing
%revisited the clause. The snapshot is now recorded (note_ctor_snapshot/1) and
%a later declaration that changes the set recompiles the clauses that read it;
%see "Constructor-set snapshots" below.
union_member_excluded(M, _, _) :- var(M), !, fail.
union_member_excluded(M, _, _) :- is_arrow_type(M), !.       %a closure is not an expression
union_member_excluded(M, _, _) :- list_type(M, _), !, fail.  %(List T) admits every length
union_member_excluded(M, N, Prior) :- is_union(M), !, M = ['|'|Ms],
                                      forall(member(M2, Ms), union_member_excluded(M2, N, Prior)).
union_member_excluded(M, N, Prior) :- is_list(M), !,
        ( tagged_tuple_type(M, Tag, FieldTs)
          -> ( length(M, N) -> length(FieldTs, K), prior_consumed_ctor(Prior, Tag, K) ; true )
           ; length(M, LM), LM =\= N ).      %positional member of a different width
union_member_excluded(M, N, Prior) :- atom(M), !,
        ( wildcard_type(M) -> fail           %Atom/Expression admit anything
        ; declared_newtype(M, R) -> union_member_excluded(R, N, Prior)
        ; primitive_type(M) -> true          %an expression is not a Number/String/Bool
        ; N =:= 0 -> true                    %() is no constructor application
        ; K is N - 1,
          note_ctor_snapshot(M),             %this verdict depends on M's constructor set
          forall(member_ctor(M, K, C), prior_consumed_ctor(Prior, C, K)) ).
union_member_excluded(_, _, _) :- fail.

%A CONSTRUCTOR of the nominal type M taking K arguments, so its applications
%have K+1 elements. PeTTa's constructor convention is the one
%declared_undefined_atom/2 (translator.pl) already implements: a declared
%symbol with NO equations stays literal data, one with equations is always
%rewritten at the call site and never survives as a value. So a declaration
%alone is not enough - \+ fun(C) is what makes C data. Counting reducible
%helpers here would only ever BLOCK an exclusion, never grant a wrong one, but
%it blocks far too much: any (= (make-goal $f $a $r) (CPU $f $a $r)) would
%stop CPU/3 from being Goal's only constructor. A wildcard output claims
%nothing, so it does not block either.
%The definedness flag is set early enough: parse_form/2 (filereader.pl)
%register_fun's every (= (F ...) ...) of a file in the parse prepass, before
%any clause of that file is compiled, so definition-below-use is fine. A
%definition arriving from a LATER file only ever unblocks an exclusion that
%was conservatively refused, and recompile_late_uses/1 revisits the clauses
%that saw the symbol as data.
member_ctor(M, K, C) :- declared_fn_type(C, ATs, OT, _), length(ATs, K),
                        \+ fun(C),
                        nonvar(OT), \+ wildcard_type_t(OT), type_compat_soft(OT, M).

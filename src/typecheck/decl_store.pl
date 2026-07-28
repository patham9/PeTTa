%%% Store maintenance, called from add_sexp/remove_sexp and forget_symbol.
%%% Caching is idempotent so seeded builtins and imports do not duplicate:
%A function type declared for a parenthesized name - (: (/?\) (-> ...)) - is a
%malformed declaration that would otherwise be ignored silently:
maybe_cache_type_decl(Space, Term) :- Space == '&self', is_list(Term), Term = [C, [Name], Type],
                                      C == (:), atom(Name),
                                      nonvar(Type), fn_type_shape(Type, _, _, _), !,
                                      format(user_error,
                                             "Warning: type declaration name (~w) is an expression; write (: ~w ...) to declare the function~n",
                                             [Name, Name]).
%Erased nominal newtypes: (: KB (Newtype Expression)) declares KB as a
%distinct compile-time role over the given representation. Nothing exists at
%runtime; the brand lives purely in the checker.
maybe_cache_type_decl(Space, Term) :- Space == '&self', is_list(Term), Term = [C, Name, [NT, R]],
                                      C == (:), atom(Name), NT == 'Newtype', !,
                                      normalize_type(R, RN),
                                      ( declared_newtype(Name, R2)
                                        -> ( R2 =@= RN -> true
                                           ; format(user_error,
                                                    "Warning: conflicting Newtype declaration for ~w ignored: ~p differs from ~p~n",
                                                    [Name, RN, R2]) )
                                      ; declared_type_alias(Name, _)
                                        -> format(user_error,
                                                  "Warning: Newtype declaration for ~w ignored: name is already an Alias~n",
                                                  [Name])
                                      ; declared_foreign_type(Name, _)
                                        -> format(user_error,
                                                  "Warning: Newtype declaration for ~w ignored: name is already a Foreign type~n",
                                                  [Name])
                                      ; declared_space_type(Name, _)
                                        -> format(user_error,
                                                  "Warning: Newtype declaration for ~w ignored: name is already a SpaceOf type~n",
                                                  [Name])
                                      ; assertz(declared_newtype(Name, RN)) ).
%Structural aliases: (: Row (Alias (Number String))) names an erased type
%expression, not a nominal role. Normalize now so later lookup is one
%non-recursive expansion. Aliases are not hoisted by precache_fn_type_decl/2;
%declare-before-use stays cheapest, while a fresh late alias repairs prior
%declarations below.
maybe_cache_type_decl(Space, Term) :- Space == '&self', is_list(Term), Term = [C, Name, [A, R]],
                                      C == (:), atom(Name), A == 'Alias', !,
                                      normalize_type(R, RN),
                                      ( declared_type_alias(Name, R2)
                                        -> ( R2 =@= RN -> true
                                           ; format(user_error,
                                                    "Warning: conflicting type alias declaration for ~w ignored: ~p differs from ~p~n",
                                                    [Name, RN, R2]) )
                                      ; declared_newtype(Name, _)
                                        -> format(user_error,
                                                  "Warning: type alias declaration for ~w ignored: name is already a Newtype~n",
                                                  [Name])
                                      ; declared_foreign_type(Name, _)
                                        -> format(user_error,
                                                  "Warning: type alias declaration for ~w ignored: name is already a Foreign type~n",
                                                  [Name])
                                      ; declared_space_type(Name, _)
                                        -> format(user_error,
                                                  "Warning: type alias declaration for ~w ignored: name is already a SpaceOf type~n",
                                                  [Name])
                                      ; assertz(declared_type_alias(Name, RN)),
                                        renormalize_late_alias(Name) ).
%Opaque foreign types: (: Heap (Foreign)) and (: Heap (Foreign 1)) declare a
%nominal runtime-uncheckable type constructor. Only its name and arity enter
%the checker; native values themselves remain wholly opaque.
maybe_cache_type_decl(Space, Term) :- Space == '&self', is_list(Term), Term = [C, Name, [F|Spec]],
                                      C == (:), atom(Name), F == 'Foreign',
                                      ( Spec == [] -> Arity = 0
                                      ; Spec = [Arity], integer(Arity), Arity > 0 ), !,
                                      ( declared_foreign_type(Name, A2)
                                        -> ( A2 == Arity -> true
                                           ; format(user_error,
                                                    "Warning: conflicting foreign type declaration for ~w ignored: arity ~w differs from ~w~n",
                                                    [Name, Arity, A2]) )
                                      ; declared_newtype(Name, _)
                                        -> format(user_error,
                                                  "Warning: foreign type declaration for ~w ignored: name is already a Newtype~n",
                                                  [Name])
                                      ; declared_type_alias(Name, _)
                                        -> format(user_error,
                                                  "Warning: foreign type declaration for ~w ignored: name is already an Alias~n",
                                                  [Name])
                                      ; declared_space_type(Name, _)
                                        -> format(user_error,
                                                  "Warning: foreign type declaration for ~w ignored: name is already a SpaceOf type~n",
                                                  [Name])
                                      ; assertz(declared_foreign_type(Name, Arity)) ).
%Typed spaces: (: &jobs (SpaceOf Row)) opts one statically named space into
%row checking. Normalize now so aliases in schemas are erased before use;
%like Newtype/Alias/Foreign, this declaration is source-ordered, not hoisted.
maybe_cache_type_decl(Space, Term) :- Space == '&self', is_list(Term), Term = [C, Name, [SO, R]],
                                      C == (:), atom(Name), SO == 'SpaceOf', !,
                                      normalize_type(R, RN),
                                      ( declared_space_type(Name, R2)
                                        -> ( R2 =@= RN -> true
                                           ; format(user_error,
                                                    "Warning: conflicting space type declaration for ~w ignored: ~p differs from ~p~n",
                                                    [Name, RN, R2]) )
                                      ; declared_newtype(Name, _)
                                        -> format(user_error,
                                                  "Warning: space type declaration for ~w ignored: name is already a Newtype~n",
                                                  [Name])
                                      ; declared_type_alias(Name, _)
                                        -> format(user_error,
                                                  "Warning: space type declaration for ~w ignored: name is already an Alias~n",
                                                  [Name])
                                      ; declared_foreign_type(Name, _)
                                        -> format(user_error,
                                                  "Warning: space type declaration for ~w ignored: name is already a Foreign type~n",
                                                  [Name])
                                      ; assertz(declared_space_type(Name, RN)) ).
maybe_cache_type_decl(Space, Term) :- ( Space == '&self', is_list(Term), Term = [C, Name, Type],
                                        C == (:), atom(Name)
                                        -> ( nonvar(Type), infix_arrow_misuse(Type)
                                             -> throw(error(infix_arrow_syntax(Name, Type), typecheck))
                                           ; nonvar(Type), fn_type_shape(Type, ATs, OT, Det)
                                             -> require_explicit_det_arrows(Name, Type),
                                                maplist(normalize_type, ATs, ATN),
                                                normalize_type(OT, OTN),
                                                remove_unexpanded_fn_precache(Name, ATs, OT, Det, ATN, OTN),
                                                note_explicit_det_decl(Name, Type, ATN),
                                                note_explicit_committed_decl(Name, Type, ATN),
                                                retractall(inferred_fn_type(Name, _, _)),  %declaration supersedes inference
                                                ( declared_fn_type(Name, A2, O2, D2),
                                                  (A2-O2-D2) =@= (ATN-OTN-Det) -> true
                                                ; assertz(declared_fn_type(Name, ATN, OTN, Det)),
                                                  enforce_late_declaration(Name),
                                                  note_constructor_set_change(Name) )
                                              ; normalize_type(Type, TN),
                                                ( declared_value_type(Name, T2), T2 =@= TN -> true
                                                ; assertz(declared_value_type(Name, TN)),
                                                  note_constructor_set_change(Name) ) )
                                         ; true ).

%Strict determinism is an explicit-effect mode: every arrow in a function
%declaration, including arrows nested in parameter/output positions (and
%aliases expanded into those positions), must name its cardinality. The
%standard builtin signature file is checker-internal type metadata; builtin
%determinism comes authoritatively from det_builtins.pl, so that one load path
%is exempt instead of duplicating hundreds of table annotations.
require_explicit_det_arrows(Name, Type) :-
    ( strict_det(true), \+ builtin_signature_load,
      normalize_type(Type, Normalized),
      type_contains_plain_arrow(Normalized)
      -> throw(error(strict_det_plain_arrow(Name), determinism))
    ; true ).

type_contains_plain_arrow(T) :-
    nonvar(T), is_list(T),
    ( T = [H|_], H == (->)
    ; member(E, T), type_contains_plain_arrow(E) ), !.

builtin_signature_load :- nb_current('$seeding_builtin_types', true), !.
builtin_signature_load :-
    current_metta_file(File),
    standard_library_path(Base),
    atomic_list_concat([Base, '/lib_builtin_types.metta'], BuiltinFile),
    catch(same_file(File, BuiltinFile), _, fail).

%Arrow declarations are pre-cached before source-ordered Alias declarations
%exist. When the declaration is processed in place, remove its syntax-only
%prepass copy if normalize_type/2 expanded an alias; otherwise that stale
%opaque overload would leak the alias name into the checker.
remove_unexpanded_fn_precache(Name, ATs, OT, Det, ATN, OTN) :-
    maplist(normalize_type_syntax, ATs, RawATs),
    normalize_type_syntax(OT, RawOT),
    ( (RawATs-RawOT) =@= (ATN-OTN)
      -> true
    ; clause(declared_fn_type(Name, A2, O2, D2), true, Ref),
      (A2-O2-D2) =@= (RawATs-RawOT-Det)
      -> erase(Ref)
    ; true ).

%Canonicalize only arrow syntax, deliberately leaving atoms untouched. This
%reconstructs the exact type cached before a source-local alias was visible.
normalize_type_syntax(T, T) :- var(T), !.
normalize_type_syntax(T, T) :- atomic(T), !.
normalize_type_syntax(T, TN) :- is_list(T), fn_type_shape(T, ATs, OT, _), !,
                                T = [Arrow|_],
                                canonical_arrow(Arrow, H),
                                maplist(normalize_type_syntax, ATs, ATN),
                                normalize_type_syntax(OT, OTN),
                                append(ATN, [OTN], Xs),
                                TN = [H|Xs].
normalize_type_syntax(T, TN) :- is_list(T), !, maplist(normalize_type_syntax, T, TN).
normalize_type_syntax(T, T).

%%% A fresh alias may arrive after declarations already cached its name as an
%%% opaque atom. Rebuild every declaration store that contains that exact atom
%%% in source order, now that normalize_type/2 can erase it. Only functions
%%% whose arrow entries changed need recompilation; determinism metadata is
%%% keyed by function name and arity, neither of which changes here.
renormalize_late_alias(Name) :-
    renormalize_alias_fn_decls(Name, Fs),
    renormalize_alias_value_decls(Name),
    renormalize_alias_space_decls(Name),
    renormalize_alias_alias_decls(Name),
    renormalize_alias_newtype_decls(Name),
    forall(member(F, Fs), enforce_late_alias_declaration(Name, F)).

type_term_mentions_alias(T, Name) :- sub_term(S, T), S == Name, !.

renormalize_alias_fn_decls(Name, Fs) :-
    findall(fn(F, ATs, OT, Det), declared_fn_type(F, ATs, OT, Det), Ds),
    findall(F, ( member(fn(F, ATs, OT, _), Ds),
                 type_term_mentions_alias(ATs-OT, Name) ), Fs0),
    sort(Fs0, Fs),
    ( Fs == [] -> true
    ; retractall(declared_fn_type(_, _, _, _)),
      forall(member(D, Ds), reassert_alias_fn_decl(Name, D)) ).

reassert_alias_fn_decl(Name, fn(F, ATs, OT, Det)) :-
    ( type_term_mentions_alias(ATs-OT, Name)
      -> maplist(normalize_type, ATs, ATN), normalize_type(OT, OTN)
    ; ATN = ATs, OTN = OT ),
    ( declared_fn_type(F, A2, O2, D2), (A2-O2-D2) =@= (ATN-OTN-Det)
      -> true ; assertz(declared_fn_type(F, ATN, OTN, Det)) ).

renormalize_alias_value_decls(Name) :-
    findall(value(V, T), declared_value_type(V, T), Ds),
    ( member(value(_, T0), Ds), type_term_mentions_alias(T0, Name)
      -> retractall(declared_value_type(_, _)),
         forall(member(value(V, T), Ds),
                ( ( type_term_mentions_alias(T, Name) -> normalize_type(T, TN) ; TN = T ),
                  ( declared_value_type(V, T2), T2 =@= TN -> true
                  ; assertz(declared_value_type(V, TN)) ) ))
    ; true ).

renormalize_alias_space_decls(Name) :-
    findall(space(S, T), declared_space_type(S, T), Ds),
    ( member(space(_, T0), Ds), type_term_mentions_alias(T0, Name)
      -> retractall(declared_space_type(_, _)),
         forall(member(space(S, T), Ds),
                ( ( type_term_mentions_alias(T, Name) -> normalize_type(T, TN) ; TN = T ),
                  ( declared_space_type(S, T2), T2 =@= TN -> true
                  ; assertz(declared_space_type(S, TN)) ) ))
    ; true ).

renormalize_alias_alias_decls(Name) :-
    findall(alias(A, T), declared_type_alias(A, T), Ds),
    ( member(alias(A0, T0), Ds), A0 \== Name, type_term_mentions_alias(T0, Name)
      -> retractall(declared_type_alias(_, _)),
         forall(member(alias(A, T), Ds),
                ( ( A \== Name, type_term_mentions_alias(T, Name)
                    -> normalize_type(T, TN) ; TN = T ),
                  assertz(declared_type_alias(A, TN)) ))
    ; true ).

renormalize_alias_newtype_decls(Name) :-
    findall(newtype(N, T), declared_newtype(N, T), Ds),
    ( member(newtype(_, T0), Ds), type_term_mentions_alias(T0, Name)
      -> retractall(declared_newtype(_, _)),
         forall(member(newtype(N, T), Ds),
                ( ( type_term_mentions_alias(T, Name) -> normalize_type(T, TN) ; TN = T ),
                  ( declared_newtype(N, T2), T2 =@= TN -> true
                  ; assertz(declared_newtype(N, TN)) ) ))
    ; true ).

enforce_late_alias_declaration(Alias, F) :-
    ( catch(nb_getval(F, [_|_]), _, fail)
      -> format(user_error,
                "Warning: type alias ~w arrives after declarations using it; recompiling ~w against the expanded type~n",
                [Alias, F]),
         recompile_function_clauses(F)
    ; true ).

%declared_fn_type/4 keeps the determinism, not the arrow that expressed it.
%The exhaustiveness check records only an EXPLICIT -[det]-> promise of exactly
%one result; plain -> is uncommitted wherever it is legal:
:- dynamic explicit_det_decl/2.

note_explicit_det_decl(Name, Type, ATs) :- ( nonvar(Type), Type = [Arrow|_], atom(Arrow),
                                             canonical_arrow(Arrow, '-[det]->'),
                                             length(ATs, N), \+ explicit_det_decl(Name, N)
                                             -> assertz(explicit_det_decl(Name, N)) ; true ).

%The boundness enforcement (src/translator.pl) and the enforced-bound
%strengthenings need BOTH committed arrows, not only -[det]->. This records the
%explicit committed determinism (det OR semidet) written on the arrow, in every
%mode - a plain -> never qualifies because only the explicit arrow is a
%per-function every-mode commitment. Kept SEPARATE from
%explicit_det_decl/2, which stays det-only for the exhaustiveness check (the
%way out of a real incompleteness is -[semidet]->, so it must not be caught).
:- dynamic explicit_committed_decl/3.   % explicit_committed_decl(F, N, Det), Det in {det, semidet}

note_explicit_committed_decl(Name, Type, ATs) :- ( nonvar(Type), Type = [Arrow|_], atom(Arrow),
                                                   canonical_arrow(Arrow, CArrow),
                                                   arrow_atom_det(CArrow, Det), committed_det(Det),
                                                   length(ATs, N), \+ explicit_committed_decl(Name, N, _)
                                                   -> assertz(explicit_committed_decl(Name, N, Det)) ; true ).

%Declaration prepass: only function (arrow) declarations are hoisted, so
%definitions may call helpers declared later in the same file. Value
%declarations stay order-sensitive - they are knowledge atoms whose position
%is meaningful (see examples/types_nondet.metta):
precache_fn_type_decl(Space, Term) :- ( is_list(Term), Term = [C, Name, Type],
                                        C == (:), atom(Name), nonvar(Type),
                                        fn_type_shape(Type, _, _, _)
                                        -> maybe_cache_type_decl(Space, Term)
                                         ; true ).

%Seed the store with the builtin operator types (called once after loading):
seed_builtin_types :- standard_library_path(Base),
                      atomic_list_concat([Base, '/lib_builtin_types.metta'], Path),
                      read_file_to_string(Path, S, []),
                      metta_string_forms(S, Forms),
                      setup_call_cleanup(
                          nb_setval('$seeding_builtin_types', true),
                          forall(member(form(FormStr, _), Forms),
                                 ( sread(FormStr, Term),
                                   maybe_cache_type_decl('&self', Term) )),
                          nb_delete('$seeding_builtin_types')).

maybe_uncache_type_decl(Space, Term) :-
    ( Space == '&self', is_list(Term), Term = [C, Name, Type],
      C == (:), atom(Name)
      -> uncache_type_decl(Name, Type)
    ; true ).

%Removal is the inverse of caching: erase the exact entry, rebuild any stores
%whose normalization depended on a removed alias, recompute explicit-arrow
%metadata from the declarations that remain in &self, and recompile affected
%clauses so cuts and guards match the surviving declarations.
uncache_type_decl(Name, [NT, R]) :- NT == 'Newtype', !,
    normalize_type(R, RN),
    ( clause(declared_newtype(Name, R2), true, Ref), R2 =@= RN
      -> affected_decl_functions([Name], Fs),
         erase(Ref),
         recompile_decl_functions(Fs)
    ; true ).
uncache_type_decl(Name, [A, R]) :- A == 'Alias', !,
    normalize_type(R, RN),
    ( clause(declared_type_alias(Name, R2), true, Ref), R2 =@= RN
      -> alias_removal_rebuild(Name, Ref)
    ; true ).
uncache_type_decl(Name, [F|Spec]) :-
    F == 'Foreign',
    ( Spec == [] -> Arity = 0
    ; Spec = [Arity], integer(Arity), Arity > 0 ), !,
    ( clause(declared_foreign_type(Name, A2), true, Ref), A2 == Arity
      -> affected_decl_functions([Name], Fs),
         erase(Ref),
         recompile_decl_functions(Fs)
    ; true ).
uncache_type_decl(Name, [SO, R]) :- SO == 'SpaceOf', !,
    normalize_type(R, RN),
    ( clause(declared_space_type(Name, R2), true, Ref), R2 =@= RN
      -> affected_decl_functions([Name], Fs),
         erase(Ref),
         recompile_decl_functions(Fs)
    ; true ).
uncache_type_decl(Name, Type) :-
    ( nonvar(Type), fn_type_shape(Type, ATs, OT, Det)
      -> maplist(normalize_type, ATs, ATN),
         normalize_type(OT, OTN),
         ( clause(declared_fn_type(Name, A2, O2, D2), true, Ref),
           (A2-O2-D2) =@= (ATN-OTN-Det)
           -> erase(Ref),
              length(ATN, N),
              recache_remaining_fn_decls(Name, N),
              recompute_explicit_decl_metadata(Name, N),
              recompile_function_clauses(Name)
         ; true )
    ; normalize_type(Type, TN),
      ( clause(declared_value_type(Name, T2), true, Ref), T2 =@= TN
        -> erase(Ref)
      ; true ) ).

recache_remaining_fn_decls(Name, N) :-
    self_type_declarations(Terms),
    forall(( member([_, Name0, Type], Terms), Name0 == Name,
             nonvar(Type), fn_type_shape(Type, ATs, _, _),
             length(ATs, N) ),
           maybe_cache_type_decl('&self', [(:), Name, Type])).

%All raw (: Name Type) atoms still present in &self, in assertion order.
self_type_declarations(Terms) :-
    findall([C, Name, Type],
            ( C = (:),
              Goal =.. ['&self', C, Name, Type],
              catch(call(Goal), _, fail) ),
            Terms).

recompute_explicit_decl_metadata(Name, N) :-
    retractall(explicit_det_decl(Name, N)),
    retractall(explicit_committed_decl(Name, N, _)),
    self_type_declarations(Terms),
    forall(( member([_, Name0, Type], Terms), Name0 == Name,
             nonvar(Type), fn_type_shape(Type, ATs, _, _),
             length(ATs, N) ),
           ( note_explicit_det_decl(Name, Type, ATs),
             note_explicit_committed_decl(Name, Type, ATs) )).

%Removing an alias must reconstruct declarations from their raw &self atoms:
%the cached stores contain only the expanded representation and cannot recover
%the alias spelling on their own. Include aliases/newtypes/spaces that depend
%on the removed name transitively, then rebuild every declaration mentioning
%one of those names in source order.
alias_removal_rebuild(Name, Ref) :-
    self_type_declarations(All),
    dependent_type_names(All, [Name], Names),
    include(declaration_mentions_any(Names), All, Terms),
    affected_decl_functions(Names, Fs0),
    findall(F, ( member(T, Terms), declaration_function_name(T, F) ), Fs1),
    append(Fs0, Fs1, Fs2), sort(Fs2, Fs),
    forall(member(T, Terms), erase_cached_declaration_only(T)),
    erase(Ref),
    forall(member(T, Terms), maybe_cache_type_decl('&self', T)),
    forall(member(F, Fs), recompute_all_explicit_metadata(F)),
    recompile_decl_functions(Fs).

dependent_type_names(All, Names0, Names) :-
    findall(N,
            ( member([_, N, Type], All),
              type_kind_representation(Type, Rep),
              member(Dep, Names0), type_term_mentions_alias(Rep, Dep) ),
            More),
    append(Names0, More, Ns0), sort(Ns0, Ns),
    ( Ns == Names0 -> Names = Ns
    ; dependent_type_names(All, Ns, Names) ).

type_kind_representation([K, R], R) :-
    ( K == 'Alias' ; K == 'Newtype' ; K == 'SpaceOf' ).

declaration_mentions_any(Names, [_, _, Type]) :-
    member(Name, Names), type_term_mentions_alias(Type, Name), !.

declaration_function_name([_, F, Type], F) :-
    nonvar(Type), fn_type_shape(Type, _, _, _).

erase_cached_declaration_only([_, Name, [NT, R]]) :- NT == 'Newtype', !,
    normalize_type(R, RN),
    ( clause(declared_newtype(Name, R2), true, Ref), R2 =@= RN -> erase(Ref) ; true ).
erase_cached_declaration_only([_, Name, [A, R]]) :- A == 'Alias', !,
    normalize_type(R, RN),
    ( clause(declared_type_alias(Name, R2), true, Ref), R2 =@= RN -> erase(Ref) ; true ).
erase_cached_declaration_only([_, Name, [F|Spec]]) :-
    F == 'Foreign',
    ( Spec == [] -> Arity = 0
    ; Spec = [Arity], integer(Arity), Arity > 0 ), !,
    ( clause(declared_foreign_type(Name, A2), true, Ref), A2 == Arity -> erase(Ref) ; true ).
erase_cached_declaration_only([_, Name, [SO, R]]) :- SO == 'SpaceOf', !,
    normalize_type(R, RN),
    ( clause(declared_space_type(Name, R2), true, Ref), R2 =@= RN -> erase(Ref) ; true ).
erase_cached_declaration_only([_, Name, Type]) :-
    ( nonvar(Type), fn_type_shape(Type, ATs, OT, Det)
      -> maplist(normalize_type, ATs, ATN), normalize_type(OT, OTN),
         ( clause(declared_fn_type(Name, A2, O2, D2), true, Ref),
           (A2-O2-D2) =@= (ATN-OTN-Det) -> erase(Ref) ; true )
    ; normalize_type(Type, TN),
      ( clause(declared_value_type(Name, T2), true, Ref), T2 =@= TN -> erase(Ref) ; true ) ).

recompute_all_explicit_metadata(F) :-
    findall(N, ( declared_fn_type(F, ATs, _, _), length(ATs, N) ), Ns0),
    sort(Ns0, Ns),
    retractall(explicit_det_decl(F, _)),
    retractall(explicit_committed_decl(F, _, _)),
    forall(member(N, Ns), recompute_explicit_decl_metadata(F, N)).

affected_decl_functions(Names, Fs) :-
    findall(F,
            ( declared_fn_type(F, ATs, OT, _),
              member(Name, Names), type_term_mentions_alias(ATs-OT, Name) ),
            ByType),
    findall(F,
            ( catch(translated_from(_, Term), _, fail), nonvar(Term),
              Term = [=, Head, _], nonvar(Head), Head = [F|_],
              member(Name, Names), type_term_mentions_alias(Term, Name) ),
            BySource),
    append(ByType, BySource, Fs0), sort(Fs0, Fs).

recompile_decl_functions(Fs) :-
    retractall(det_analysis_cache(_, _, _)),
    retractall(det_assume_cache(_, _, _)),
    reset_output_certs(_),
    forall(member(F, Fs), recompile_function_clauses(F)).

%%% A declaration that arrives after the function was compiled used to be
%%% BELIEVED and never ENFORCED: the clauses were validated (and emitted) with
%%% no declaration in sight, so a late -[det]-> got no overlap check, no body
%%% determinism check and no commit cut, while every later caller was told the
%%% function is det. A warning is not enough for that - the compiler was
%%% asserting something it had not checked.
%%%
%%% This cannot happen INSIDE a file: process_metta_string/3 pre-caches every
%%% arrow declaration of a file before compiling any of its definitions. It is
%%% specifically a cross-file (or add-atom-at-runtime) situation, so recompiling
%%% is cheap - it revisits one function, not the program.
%%%
%%% Recompiling, rather than rejecting, is what a declaration prepass would
%%% have done had the two files been one, so it is the semantics that already
%%% exists rather than a new rule. The clauses go back through
%%% translate_clause/2 with the declaration in place: they get their argument
%%% and output certifications, their determinism verdict, and their commit cut,
%%% and if they cannot satisfy the declaration the normal error is thrown.
%%% The warning stays, because a recompile can change a program that already
%%% ran part of itself.
enforce_late_declaration(Name) :-
    ( catch(nb_getval(Name, [_|_]), _, fail)
      -> format(user_error,
                "Warning: type declaration for ~w arrives after its definition; its clauses are being recompiled against it~n",
                [Name]),
         recompile_function_clauses(Name)
       ; true ).

forget_symbol_types(Name) :- retractall(declared_fn_type(Name, _, _, _)),
                             retractall(explicit_det_decl(Name, _)),
                             retractall(explicit_committed_decl(Name, _, _)),
                             retractall(declared_value_type(Name, _)),
                             retractall(declared_newtype(Name, _)),
                             retractall(declared_type_alias(Name, _)),
                             retractall(declared_foreign_type(Name, _)),
                             retractall(declared_space_type(Name, _)),
                             retractall(inferred_fn_type(Name, _, _)),
                             retractall(det_bound_proviso(Name, _, _, _)),
                             reset_output_certs(Name).  %withdraw the output certificates

%%% Store lookup (each retrieval yields a fresh copy of the declaration):
fn_decl_arity(F, N, ATs, OT) :- declared_fn_type(F, ATs, OT, _), length(ATs, N).
fn_decl_partial(F, N, PTs, RTs, OT) :- fn_decl_partial(F, N, PTs, RTs, OT, _).
fn_decl_partial(F, N, PTs, RTs, OT, Det) :- declared_fn_type(F, ATs, OT, Det),
                                            length(ATs, Total), Total > N,
                                            length(PTs, N), append(PTs, RTs, ATs).

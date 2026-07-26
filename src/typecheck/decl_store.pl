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
                                      ( declared_newtype(Name, R2), R2 =@= RN -> true
                                                                              ; assertz(declared_newtype(Name, RN)) ).
maybe_cache_type_decl(Space, Term) :- ( Space == '&self', is_list(Term), Term = [C, Name, Type],
                                        C == (:), atom(Name)
                                        -> ( nonvar(Type), infix_arrow_misuse(Type)
                                             -> throw(error(infix_arrow_syntax(Name, Type), typecheck))
                                           ; nonvar(Type), fn_type_shape(Type, ATs, OT, Det)
                                             -> maplist(normalize_type, ATs, ATN),
                                                normalize_type(OT, OTN),
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

%declared_fn_type/4 keeps the determinism, not the arrow that expressed it, and
%under --strict-det a plain -> also yields det. The exhaustiveness check needs
%the difference: an EXPLICIT -[det]-> is a per-function promise of exactly one
%result, a plain -> is a mode-wide default. Only the former is recorded here:
:- dynamic explicit_det_decl/2.

note_explicit_det_decl(Name, Type, ATs) :- ( nonvar(Type), Type = [Arrow|_], atom(Arrow),
                                             canonical_arrow(Arrow, '-[det]->'),
                                             length(ATs, N), \+ explicit_det_decl(Name, N)
                                             -> assertz(explicit_det_decl(Name, N)) ; true ).

%The boundness enforcement (src/translator.pl) and the enforced-bound
%strengthenings need BOTH committed arrows, not only -[det]->. This records the
%explicit committed determinism (det OR semidet) written on the arrow, in every
%mode - a plain -> never qualifies, not even under --strict-det, because only
%the explicit arrow is a per-function every-mode commitment. Kept SEPARATE from
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
                      forall(member(form(FormStr, _), Forms),
                             ( sread(FormStr, Term),
                               maybe_cache_type_decl('&self', Term) )).

maybe_uncache_type_decl(Space, Term) :- ( Space == '&self', is_list(Term), Term = [C, Name, Type],
                                          C == (:), atom(Name)
                                          -> ( nonvar(Type), fn_type_shape(Type, ATs, OT, Det)
                                               -> maplist(normalize_type, ATs, ATN),
                                                  normalize_type(OT, OTN),
                                                  ( clause(declared_fn_type(Name, A2, O2, D2), true, Ref),
                                                    (A2-O2-D2) =@= (ATN-OTN-Det)
                                                    -> erase(Ref),
                                                       length(ATN, NA),
                                                       ( declared_fn_type(Name, A3, _, _), length(A3, NA)
                                                         -> true ; retractall(explicit_det_decl(Name, NA)),
                                                                   retractall(explicit_committed_decl(Name, NA, _)) )
                                                     ; true )
                                                ; normalize_type(Type, TN),
                                                  ( clause(declared_value_type(Name, T2), true, Ref),
                                                    T2 =@= TN
                                                    -> erase(Ref) ; true ) )
                                           ; true ).

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
                             retractall(inferred_fn_type(Name, _, _)),
                             retractall(det_bound_proviso(Name, _, _)).

%%% Store lookup (each retrieval yields a fresh copy of the declaration):
fn_decl_arity(F, N, ATs, OT) :- declared_fn_type(F, ATs, OT, _), length(ATs, N).
fn_decl_partial(F, N, PTs, RTs, OT) :- fn_decl_partial(F, N, PTs, RTs, OT, _).
fn_decl_partial(F, N, PTs, RTs, OT, Det) :- declared_fn_type(F, ATs, OT, Det),
                                            length(ATs, Total), Total > N,
                                            length(PTs, N), append(PTs, RTs, ATs).


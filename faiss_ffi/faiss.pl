module(faiss,
    [ faiss_create/2,
      faiss_free/1,
      faiss_add/2,
      faiss_search/4
    ]).

:- initialization(faiss_init).
faiss_init :- working_dir(Base),
              atomic_list_concat([Base, '/faisslib.so'], Path),
              use_foreign_library(Path).

:- dynamic faiss_next_id/2.      % faiss_next_id(Index, NextIntId)
:- dynamic faiss_atom_id/3.      % faiss_atom_id(Index, Atom, IntId)
:- dynamic faiss_id_vec/3.       % faiss_id_vec(Index, IntId, VecList)
:- dynamic faiss_index_dim/2.    % faiss_index_dim(Index, Dim).

%% faiss_create(+Dim, -Index)
faiss_create(Dim, Index) :-
    faiss_create_c(Dim, Index),
    assertz(faiss_next_id(Index, 0)),
    assertz(faiss_index_dim(Index, Dim)).

%% faiss_free(+Index)
faiss_free(Index) :-
    faiss_free_c(Index),
    retractall(faiss_next_id(Index, _)),
    retractall(faiss_atom_id(Index, _, _)),
    retractall(faiss_id_vec(Index, _, _)),
    retractall(faiss_index_dim(Index, _)).

%% faiss_add(+Index, +Pairs)
% Pairs = [[Atom, [F1,F2,...]], ...]
faiss_add(Index, Pairs, true) :-
    faiss_next_id(Index, Next0),
    collect_ids_vectors(Pairs, Index, Next0, NextN, Ids, Flat),
    retractall(faiss_next_id(Index, _)),
    assertz(faiss_next_id(Index, NextN)),
    faiss_add_with_ids_c(Index, Flat, Ids).

collect_ids_vectors([], _Index, Next, Next, [], []).
collect_ids_vectors([[Atom,Vec]|Rest], Index, Id0, IdN, [Id0|Ids], FlatAll) :-
    assertz(faiss_atom_id(Index, Atom, Id0)),
    assertz(faiss_id_vec(Index, Id0, Vec)),
    Id1 is Id0 + 1,
    collect_ids_vectors(Rest, Index, Id1, IdN, Ids, FlatRest),
    append(Vec, FlatRest, FlatAll).

%% faiss_search(+Index, +QueryVec, +K, -Results)
% Results = [Atom-Dist, ...]
faiss_search(Index, QueryVec, K, Results) :-
    faiss_search_c(Index, QueryVec, K, result(Dists, Ids)),
    map_ids_to_atoms(Index, Ids, Dists, Results).

map_ids_to_atoms(_, [], [], []).
map_ids_to_atoms(Index, [Id|Ids], [D|Ds], [[Atom,D]|Rest]) :-
    faiss_atom_id(Index, Atom, Id), !,
    map_ids_to_atoms(Index, Ids, Ds, Rest).
map_ids_to_atoms(Index, [_|Ids], [_|Ds], Rest) :-
    map_ids_to_atoms(Index, Ids, Ds, Rest).

%% faiss_remove(+Index, +Atom)
faiss_remove(Index, Atom) :-
    faiss_atom_id(Index, Atom, Id),
    faiss_remove_ids_c(Index, [Id]),
    retractall(faiss_atom_id(Index, Atom, _)),
    retractall(faiss_id_vec(Index, Id, _)).

%% faiss_rebuild(+OldIndex, -NewIndex)
% Rebuilds a compact index to reclaim RAM.
faiss_rebuild(OldIndex, NewIndex) :-
    faiss_index_dim(OldIndex, Dim),
    findall([Id,Atom,Vec],
            ( faiss_atom_id(OldIndex, Atom, Id),
              faiss_id_vec(OldIndex, Id, Vec)
            ),
        Triples),
    faiss_create(Dim, NewIndex),
    % copy next-id counter
    ( faiss_next_id(OldIndex, NextOld)
    -> retractall(faiss_next_id(NewIndex, _)),
       assertz(faiss_next_id(NewIndex, NextOld))
    ;  true
    ),
    triples_to_ids_flat(Triples, NewIndex, Ids, Flat),
    ( Ids \= [] ->
        faiss_add_with_ids_c(NewIndex, Flat, Ids)
    ;   true
    ),
    % move mappings to new index
    forall(member([Id,Atom,Vec], Triples),
           ( retractall(faiss_atom_id(OldIndex, Atom, Id)),
             retractall(faiss_id_vec(OldIndex, Id, Vec)),
             assertz(faiss_atom_id(NewIndex, Atom, Id)),
             assertz(faiss_id_vec(NewIndex, Id, Vec))
           )),
    retractall(faiss_index_dim(OldIndex, _)),
    retractall(faiss_next_id(OldIndex, _)),
    faiss_free_c(OldIndex).

triples_to_ids_flat([], _NewIndex, [], []).
triples_to_ids_flat([[Id,Atom,Vec]|Rest], NewIndex, [Id|Ids], FlatAll) :-
    assertz(faiss_atom_id(NewIndex, Atom, Id)),
    assertz(faiss_id_vec(NewIndex, Id, Vec)),
    triples_to_ids_flat(Rest, NewIndex, Ids, FlatRest),
    append(Vec, FlatRest, FlatAll).

/** <module> Structural symbolic embedding for nested lists and terms.

This module produces stable, similarity-preserving embeddings for
arbitrary symbolic expressions, including deeply nested lists.

Now supports: embed(+Expr,+Dim,-Vec)
*/

:- module(embed_dim,
    [ embed/2,          % +Expr, -Vec (default dimension=64)
      embed/3,          % +Expr, +Dim, -Vec
      clear_symvec/0    % to reset symbol vectors
    ]).

:- dynamic sym_vec/3.
% sym_vec(Dim, Symbol, Vec)


% ============================================================
% Configuration
% ============================================================

default_dim(64).

rand_range(0.2).       % scale of random variations


% ============================================================
% Utility
% ============================================================

clear_symvec :-
    retractall(sym_vec(_,_,_)).


% ============================================================
% Random vector generation
% ============================================================

random_float_signed(X) :-
    rand_range(R),
    random(U),
    X is (U - 0.5) * R.

random_vec(Dim, Vec) :-
    length(Vec, Dim),
    maplist(random_float_signed, Vec).


% ============================================================
% Persistent symbol -> vector (dimension-aware)
% ============================================================

sym_vector(Dim, Sym, Vec) :-
    sym_vec(Dim, Sym, Vec),
    !.
sym_vector(Dim, Sym, Vec) :-
    random_vec(Dim, Vec),
    assertz(sym_vec(Dim, Sym, Vec)).


% ============================================================
% Vector ops
% ============================================================

vec_add([], [], []).
vec_add([A|As], [B|Bs], [C|Cs]) :-
    C is A + B,
    vec_add(As, Bs, Cs).

scale_vec([], _, []).
scale_vec([X|Xs], S, [Y|Ys]) :-
    Y is X * S,
    scale_vec(Xs, S, Ys).


% ============================================================
% Public API
% ============================================================

% Default dimension version
embed(Expr, Vec) :-
    default_dim(D),
    embed(Expr, D, Vec).

% Full version
embed(Expr, Dim, Vec) :-
    embed0(Expr, Dim, V0),
    normalize(V0, Vec).


% ============================================================
% Core embedding recursion
% ============================================================

embed0(X, Dim, Vec) :-
    atomic(X),
    !,
    sym_vector(Dim, X, Vec).

embed0(List, Dim, Vec) :-
    is_list(List),
    !,
    sym_vector(Dim, list, Base),
    embed_list(List, Dim, Base, Vec).

embed0(Term, Dim, Vec) :-
    Term =.. [F|Args],
    sym_vector(Dim, F, Base),
    embed_args(Args, Dim, Base, Vec).


embed_list([], _, Vec, Vec).
embed_list([X|Xs], Dim, Acc, Vec) :-
    embed0(X, Dim, VX),
    vec_add(Acc, VX, Acc2),
    embed_list(Xs, Dim, Acc2, Vec).

embed_args([], _, Vec, Vec).
embed_args([A|As], Dim, Acc, Vec) :-
    embed0(A, Dim, VA),
    vec_add(Acc, VA, Acc2),
    embed_args(As, Dim, Acc2, Vec).


% ============================================================
% Normalization
% ============================================================

normalize(V, VN) :-
    norm(V, N),
    (   N =:= 0 -> VN = V
    ;   scale_vec(V, 1.0/N, VN)
    ).

norm(Vec, N) :-
    sumsq(Vec, S),
    N is sqrt(S).

sumsq([], 0).
sumsq([X|Xs], S) :-
    sumsq(Xs, S2),
    S is S2 + X*X.

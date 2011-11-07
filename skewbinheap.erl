%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(skewbinheap).

-export([new/0,
         is_empty/1,
         merge/2,
         insert/2,
         insert_all/2,
         min/1,
         delete_min/1,
         find_delete_min/1]).

%% A tree is an integer, a value, a list of values, and a list of child trees.
-type(tree(A) :: {integer(), A, [A], [tree(A)]}).

%% A heap is a list of trees.
-type heap(A) :: [tree(A)].

%% @doc Create a new heap.
-spec new() -> heap(_).
new() ->
    [].

%% @doc Is the given heap empty?
-spec is_empty(heap(_)) -> boolean().
is_empty([]) ->
    true;
is_empty(_) ->
    false.

%% Link two trees together.
-spec link(tree(X), tree(Y)) -> tree(X | Y).
link({R, X1, Xs1, C1} = T1, {_, X2, Xs2, C2} = T2) ->
    if 
        X1 =< X2 ->
            {R + 1, X1, Xs1, [T2 | C1]};
        true ->
            {R + 1, X2, Xs2, [T1 | C2]}
    end.

%% Link T1 and T2 together with a new root X.
-spec skew_link(X, tree(Y), tree(Z)) -> tree(X | Y | Z).
skew_link(X, T1, T2) ->
    {R, Y, Ys, C} = link(T1, T2),
    if 
        X =< Y ->
            {R, X, [Y | Ys], C};
        true ->
            {R, Y, [X | Ys], C}
    end.

%% Insert a tree into a heap.
-spec insert_tree(tree(X), heap(Y)) -> heap(X | Y).
insert_tree(T, []) ->
    [T];
insert_tree({R1, _, _, _} = T1, [{R2, _, _, _} = T2 | Ts] = H) ->
    if
        R1 < R2 ->
            [T1 | H];
        true ->
            insert_tree(link(T1, T2), Ts)
    end.

%% Merge two heaps together.
-spec merge_trees(heap(X), heap(Y)) -> heap(X | Y).
merge_trees(H, []) ->
    H;
merge_trees([], H) ->
    H;
merge_trees([{RX, _, _, _} = X | Xs] = H1, [{RY, _, _, _} = Y | Ys] = H2) ->
    if
        RX < RY ->
            %% Prepend X
            [X | merge_trees(Xs, H2)];
        RX > RY ->
            %% Prepend Y
            [Y | merge_trees(H1, Ys)];
        true ->
            %% Equal ranks
            insert_tree(link(X, Y), merge_trees(Xs, Ys))
    end.

-spec normalize(heap(X)) -> heap(X).
normalize([]) ->
    [];
normalize([Head | Tail]) ->
    insert_tree(Head, Tail).

%% @doc Insert an object into the heap.
-spec insert(X, heap(Y)) -> heap(X | Y).
insert(X, [{R1, _, _, _} = T1 | [{R2, _, _, _} = T2 | Rest]] = Ts) ->
    if
        R1 == R2 ->
            [skew_link(X, T1, T2) | Rest];
        true ->
            [{0, X, [], []} | Ts]
    end;
insert(X, Ts) ->
    %% Simply prepend a single-element tree.
    [{0, X, [], []} | Ts].

%% Merge two heaps.
-spec merge(heap(X), heap(Y)) -> heap(X | Y).
merge(T1, T2) ->
    merge_trees(normalize(T1), normalize(T2)).

%% Remove minimum tree from a heap.
-spec remove_min_tree(heap(X)) -> {tree(X), heap(X)} | no_return().
remove_min_tree([]) ->
    throw(empty);
remove_min_tree([T]) ->
    {T, []};
remove_min_tree([{_, V, _, _} = T | Ts]) ->
    {{_, V1, _, _} = T1, Ts1} = remove_min_tree(Ts),
    if
        V =< V1 ->
            {T, Ts};
        true ->
            {T1, [T | Ts1]}
    end.

%% @doc Find minimum element.
-spec min(heap(X)) -> X.
min(Ts) ->
    {{_, V, _, _}, _} = remove_min_tree(Ts),
    V.

%% @doc Insert a list of elements.
-spec insert_all([X], heap(Y)) -> heap(X | Y).
insert_all([], Ts) ->
    Ts;
insert_all([X | Xs], Ts) ->
    insert_all(Xs, insert(X, Ts)).

%% @doc Delete minimum element.
-spec delete_min(heap(X)) -> heap(X).
delete_min(Ts) ->
    {_Min, Deleted} = find_delete_min(Ts),
    Deleted.

%% @doc Finds and deletes minimum element.
%% @doc Returns {Min, HeapWithoutMin}
-spec find_delete_min(heap(X)) -> {X, heap(X)}.
find_delete_min(Ts) ->
    {{_, X, Xs, Ts1}, Ts2} = remove_min_tree(Ts),
    {X, insert_all(Xs, merge(lists:reverse(Ts1), Ts2))}.


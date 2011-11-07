%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(skewbinheap_proper).

-include_lib("proper/include/proper.hrl").

-compile(export_all).

% Deconstruct a heap in sorted order. n log n. :(
ary([]) ->
    [];
ary(Heap) ->
    {X, Heap1} = skewbinheap:find_delete_min(Heap),
    [X | ary(Heap1)].

% Test that inserting elements returns the least, or throws empty.
prop_min() ->
    ?FORALL({Xs}, {list(int())},
        begin
            H = skewbinheap:insert_all(Xs, skewbinheap:new()),
            try
                equals(skewbinheap:min(H), lists:min(Xs))
            catch
                throw:empty ->
                    length(Xs) == 0
            end
        end).

test_min(N) ->
    quickcheck(numtests(N, prop_min())).

% Test that successive minimum elements are sorted.
prop_sort() ->
    ?FORALL({Xs}, {list(int())},
        begin
            H = skewbinheap:insert_all(Xs, skewbinheap:new()),
            equals(ary(H), lists:sort(Xs))
        end).

test_sort(N) ->
    quickcheck(numtests(N, prop_sort())).

% Test that merging two heaps results in the addition of their lists.
prop_merge() ->
    ?FORALL({Xs, Ys}, {list(int), list(int)},
        begin
            H1 = skewbinheap:insert_all(Xs, skewbinheap:new()),
            H2 = skewbinheap:insert_all(Ys, skewbinheap:new()),
            H = skewbinheap:merge(H1, H2),
            equals(ary(H), lists:sort(Xs ++ Ys))
        end).

test_merge(N) ->
    quickcheck(numtests(N, prop_merge())).


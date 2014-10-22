-module(flow_network).

-export([
         ford_fulkerson/3,
         undirect/1
        ]).

%% @doc Function for undirected graph
-spec undirect(#{atom() => #{atom() => {number(), number()}}}) ->
    #{atom() => #{atom() => {number(), number()}}}.
undirect(G) ->
    maps:fold(
      fun(V1, Child, NewG) ->
              maps:fold(
                fun(V2, {W1, W2}, NewNewG) ->
                        OldChild = maps:get(V2, NewNewG),
                        maps:put(V2, maps:put(V1, {W2, W1}, OldChild), NewNewG)
                end, NewG, Child)
      end, G, G).

%% @doc Implement ford-falkerson algorithm
-spec ford_fulkerson(#{atom() => #{atom() => {number(), number()}}},
                     atom(), atom()) -> number().
ford_fulkerson(G, Start, Stop) ->
    ford_fulkerson(undirect(G), Start, Stop, 0).

ford_fulkerson(G, Start, Stop, Bandwidth) ->
    case search:bfs(G, Start, Stop) of
        {MinBand, P = [Start | Path]} ->
            io:format("Minand: ~n~p~nMinPath: ~n~p~n", [MinBand, P]),
            {_, NG} = lists:foldl(
              fun(V2, {V1, AccG}) ->
                      NewG = ford_fulkerson_step(AccG, V1, V2, MinBand),
                      {V2, NewG}
              end, {Start, G}, Path),
            io:format("NewG: ~n~p~n"
                      "==============~n", [NG]),
            ford_fulkerson(NG, Start, Stop, Bandwidth + MinBand);
        none ->
            Bandwidth
    end.

ford_fulkerson_step(G, V1, V2, MinBand) ->
    ChildG = maps:get(V1, G),
    {V3, V4, W3, W4} = case maps:get(V2, ChildG) of
        {W1, W2} when W1 < W2 ->
            {V1, V2, W1 + MinBand, W2};
        {W1, W2} ->
            {V2, V1, W1, W2 + MinBand}
    end,
    if W3 - W4 == 0 ->
           remove_edge(G, V3, V4);
       true ->
           update_edge(G, V3, V4, {W3, W4})
    end.

remove_edge(G, V1, V2) ->
    ChildG = maps:get(V1, G),
    NG = maps:put(V1, maps:remove(V2, ChildG), G),
    maps:put(V2, maps:remove(V1, maps:get(V2, NG)), NG).

update_edge(G, V1, V2, {W1, W2}) ->
    ChildG = maps:get(V1, G),
    NG = maps:put(V1, maps:put(V2, {W1, W2}, ChildG), G),
    maps:put(V2, maps:put(V1, {W2, W1}, maps:get(V2, NG)), NG).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ford_fulkerson_test() ->
    {Start, Stop, Map} = graph:read_config("../example/graph1"),
    {Start2, Stop2, Map2} = graph:read_config("../example/graph2"),
    [
     ?assertEqual(ford_fulkerson(Map, Start, Stop), 2000),
     ?assertEqual(ford_fulkerson(Map, Stop, Start), 2000),
     ?assertEqual(ford_fulkerson(Map2, Start2, Stop2), 1001)
    ].

remove_edge_test() ->
    {_, _, Map} = graph:read_config("../example/graph1"),
    UG = undirect(Map),
    [
     ?assertEqual(remove_edge(UG, a, b), #{
                    a => #{ c => {1000,0}},
                    b => #{ c => {1,0}, d => {1000,0}},
                    c => #{ a => {0,1000}, b => {0,1}, d => {1000,0}},
                    d => #{ b => {0,1000}, c => {0,1000}}
                   }),
     ?assertEqual(remove_edge(
                    #{a => #{c => {1000,500}},
                      b => #{c => {1,0},d => {1000,501}},
                      c => #{a => {500,1000},b => {0,1}},
                      d => #{b => {501,1000}}},
                    b, c),
                    #{a => #{c => {1000,500}},
                      b => #{d => {1000,501}},
                      c => #{a => {500,1000}},
                      d => #{b => {501,1000}}}),
     ?assertException(error, bad_key, remove_edge(UG, e, d))
    ].

update_edge_test() ->
    {_, _, Map} = graph:read_config("../example/graph1"),
    UG = undirect(Map),
    [
     ?assertEqual(update_edge(UG, a, b, {20, 30}), #{
                    a => #{ b => {20,30}, c => {1000,0}},
                    b => #{ a => {30,20}, c => {1,0}, d => {1000,0}},
                    c => #{ a => {0,1000}, b => {0,1}, d => {1000,0}},
                    d => #{ b => {0,1000}, c => {0,1000}}
                   }),
     ?assertException(error, bad_key, update_edge(UG, e, d, {20, 30}))
    ].
-endif. %%TEST

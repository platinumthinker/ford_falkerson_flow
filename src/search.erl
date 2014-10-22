-module(search).
-export([bfs/3]).

%% @doc Breadth-first search
-spec bfs(#{ atom() => #{atom => {number(), number()} } },
          [atom()], atom()) ->
    {number(), list(V :: atom())}.
bfs(G, Start, Stop) when is_atom(Start) ->
    bfs(G, [Start], Stop);
bfs(G, Start, Stop) ->
    LQueue = [ {V, none, []} || V <- Start ],
    Q = queue:from_list(LQueue),
    case bfs_step(G, Stop, Q) of
        {MinBandwidth, Path} ->
            {MinBandwidth, lists:reverse(Path)};
        none ->
            none
    end.

bfs_step(_, V, {_, [{V, MinBandwidth, Path} | _]}) ->
    {MinBandwidth, [V | Path]};
bfs_step(_, V, {[{V, MinBandwidth, Path} | _], _}) ->
    {MinBandwidth, [V | Path]};
bfs_step(G, Stop, Queue) ->
    case queue:out(Queue) of
        {{value, {V, MinBand, Path}}, Q} ->
            Child = maps:get(V, G),
            NewQ = maps:fold(fun(V1, {W1, W2}, AccQ) ->
                NewBand = min(MinBand, abs(W1 - W2)),
                Item = {V1, NewBand, [V | Path]},
                queue:in(Item, AccQ)
            end, Q, maps:without(Path, Child)),
            bfs_step(G, Stop, NewQ);
        {empty, _} ->
            none
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

bfs_test() ->
    {Start, Stop, Map} = graph:read_config("../example/graph1"),
    UG = flow_network:undirect(Map),
    UG_true = #{a => #{ b => {1000,0}, c => {1000,0}},
                b => #{ a => {0,1000}, c => {1,0}, d => {1000,0}},
                c => #{ a => {0,1000}, b => {0,1}, d => {1000,0}},
                d => #{ b => {0,1000}, c => {0,1000}}},
    UG2 = #{a => #{c => {1000,500}},
            b => #{c => {1,0},d => {1000,500}},
            c => #{a => {500,1000},b => {0,1}},
            d => #{b => {500,1000}}},
    [
     ?assertEqual(graph:bfs(Map, Start, Stop), {1000, [a, b, d]}),
     ?assertEqual(graph:bfs(UG, Start, Stop), {1000, [a, b, d]}),
     ?assertEqual(graph:bfs(UG, Start, e), none),
     ?assertEqual(UG, UG_true),
     ?assertEqual(graph:bfs(UG, Stop, Start), {1000, [d, b, a]}),
     ?assertEqual(graph:bfs(UG2, Start, Stop), {1, [a, c, b, d]}),
     ?assertEqual(graph:bfs(UG2, Stop, Start), {1, [d, b, c, a]})
    ].

-endif. %%TEST

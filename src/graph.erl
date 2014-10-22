-module(graph).
-mode(compile).

-export([
         main/1,
         read_config/1,
         undirect/1,
         bfs/3,
         ford_fulkerson/3,
         ford_fulkerson_step/4,
         remove_edge/3,
         update_edge/4
        ]).

main(Args) ->
    {Start, Stop, Map} = read_config(Args),
    Res = ford_fulkerson(Map, Start, Stop),
    io:format("Max flow: ~p~n", [Res]).

-spec read_config(File :: file:filename()) ->
    {Start :: atom(), Stop :: atom(), map()}.
read_config(File) ->
    {ok, Term} = file:consult(File),
    [{Start, Stop} | G] = Term,
    NewG = lists:foldl(fun({V1, Child}, Acc) ->
        NChild = [ {V2, {W1, 0}} || {V2, W1} <- Child ],
        Item = maps:from_list(NChild),
        maps:put(V1, Item, Acc)
            end, maps:new(), G),
    {Start, Stop, NewG}.

undirect(G) ->
    maps:fold(
      fun(V1, Child, NewG) ->
              maps:fold(
                fun(V2, {W1, W2}, NewNewG) ->
                        OldChild = maps:get(V2, NewNewG),
                        maps:put(V2, maps:put(V1, {W2, W1}, OldChild), NewNewG)
                end, NewG, Child)
      end, G, G).


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
bfs_step(G, Stop, Queue) ->
    case queue:out(Queue) of
        {{value, {V, MinBand, Path}}, Q} ->
            Child = maps:get(V, G),
            NewQ = maps:fold(fun(V1, {W1, W2}, AccQ) ->
                IsMember = lists:member(V1, Path),
                if IsMember ->
                    AccQ;
                true ->
                    NewBand = min(MinBand, abs(W1 - W2)),
                    Item = {V1, NewBand, [V | Path]},
                    queue:in(Item, AccQ)
                end end, Q, Child),
            bfs_step(G, Stop, NewQ);
        {empty, _} ->
            none
    end.

ford_fulkerson(G, Start, Stop) ->
    UG = undirect(G),
    io:format("Start ford fulkerson with: ~n~p~n", [UG]),
    ford_fulkerson(UG, Start, Stop, 0).

ford_fulkerson(G, Start, Stop, Bandwidth) ->
    case bfs(G, Start, Stop) of
        {MinBand, P = [Start | Path]} ->
            io:format("Minand: ~n~p~nMinPath: ~n~p~n", [MinBand, P]),
            {_, NG} = lists:foldl(
              fun(V2, {V1, AccG}) ->
                      NewG = ford_fulkerson_step(AccG, V1, V2, MinBand),
                      io:format("NewG step: ~p => ~p ~n~30p~n", [V1, V2, NewG]),
                      {V2, NewG}
              end, {Start, G}, Path),
            io:format("NewG: ~n~p~n", [NG]),
            ford_fulkerson(NG, Start, Stop, Bandwidth + MinBand);
        none ->
            Bandwidth
    end.

ford_fulkerson_step(G, V1, V2, MinBand) ->
    case maps:find(V1, G) of
        {ok, ChildG} ->
            {W1, W2} = maps:get(V2, ChildG),
            NewW = W2 + MinBand,
            if abs(W1 - NewW) == 0 ->
                   remove_edge(G, V1, V2, ChildG);
               true ->
                   update_edge(G, V1, V2, ChildG, {W1, NewW})
            end;
        error ->
            ChildG = maps:get(V2, G),
            {W1, W2} = maps:get(V1, ChildG),
            NewW = W2 - MinBand,
            if abs(W1 - NewW) == 0 ->
                   remove_edge(G, V2, V1, ChildG);
               true ->
                   update_edge(G, V2, V1, ChildG, {W1, NewW})
            end
    end.

remove_edge(G, V1, V2) ->
    remove_edge(G, V1, V2, maps:get(V1, G)).
remove_edge(G, V1, V2, ChildG) ->
    NG = maps:put(V1, maps:remove(V2, ChildG), G),
    maps:put(V2, maps:remove(V1, maps:get(V2, NG)), NG).

update_edge(G, V1, V2, Val) ->
    update_edge(G, V1, V2, maps:get(V1, G), Val).
update_edge(G, V1, V2, ChildG, {W1, W2}) ->
    NG = maps:put(V1, maps:put(V2, {W1, W2}, ChildG), G),
    NUG = maps:put(V1, maps:put(V2, {W1, W2}, ChildG), NG),
    maps:put(V2, maps:put(V1, {W2, W1}, maps:get(V2, NUG)), NUG).

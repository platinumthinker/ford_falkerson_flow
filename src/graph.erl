-module(graph).
-mode(compile).

-export([
         main/1,
         read_config/1
        ]).

main(Args) ->
    {Start, Stop, Map} = read_config(Args),
    Res = flow_network:ford_fulkerson(Map, Start, Stop),
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

-module(bfs_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

bfs_test() ->
    {Start, Stop, Map} = graph:read_config("../test/example/graph1"),
    UG = graph:undirect(Map),
    [
     ?assertEqual(graph:bfs(Map, Start, Stop), {1000, [a, b, d]}),
     ?assertEqual(graph:bfs(UG, Start, Stop), {1000, [a, b, d]}),
     ?assertEqual(graph:bfs(UG, Start, e), none),
     ?assertEqual(UG, #{
                    a => #{ b => {1000,0}, c => {1000,0}},
                    b => #{ a => {0,1000}, c => {1,0}, d => {1000,0}},
                    c => #{ a => {0,1000}, b => {0,1}, d => {1000,0}},
                    d => #{ b => {0,1000}, c => {0,1000}}
                   }),
     ?assertEqual(graph:bfs(UG, Stop, Start), {1000, [d, b, a]})
    ].

ford_fulkerson_test() ->
    {Start, Stop, Map} = graph:read_config("../test/example/graph1"),
    ?assertEqual(graph:ford_fulkerson(Map, Start, Stop), 2000).

remove_edge_test() ->
    {_, _, Map} = graph:read_config("../test/example/graph1"),
    UG = graph:undirect(Map),
    [
     ?assertEqual(graph:remove_edge(UG, a, b), #{
                    a => #{ c => {1000,0}},
                    b => #{ c => {1,0}, d => {1000,0}},
                    c => #{ a => {0,1000}, b => {0,1}, d => {1000,0}},
                    d => #{ b => {0,1000}, c => {0,1000}}
                   }),
     ?assertException(error, bad_key, graph:remove_edge(UG, e, d))
    ].

update_edge_test() ->
    {_, _, Map} = graph:read_config("../test/example/graph1"),
    UG = graph:undirect(Map),
    [
     ?assertEqual(graph:update_edge(UG, a, b, {20, 30}), #{
                    a => #{ b => {20,30}, c => {1000,0}},
                    b => #{ a => {30,20}, c => {1,0}, d => {1000,0}},
                    c => #{ a => {0,1000}, b => {0,1}, d => {1000,0}},
                    d => #{ b => {0,1000}, c => {0,1000}}
                   }),
     ?assertException(error, bad_key, graph:update_edge(UG, e, d, {20, 30}))
    ].


-module(bfs_test).

-include_lib("eunit/include/eunit.hrl").

bfs_test() ->
    {Start, Stop, Map} = graph:read_config("../test/example/graph1"),
    UG = graph:undirect(Map),
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

ford_fulkerson_test() ->
    {Start, Stop, Map} = graph:read_config("../test/example/graph1"),
    {Start2, Stop2, Map2} = graph:read_config("../test/example/graph2"),
    [
     ?assertEqual(graph:ford_fulkerson(Map, Start, Stop), 2000),
     ?assertEqual(graph:ford_fulkerson(Map, Stop, Start), 2000),
     ?assertEqual(graph:ford_fulkerson(Map2, Start2, Stop2), 1001)
    ].

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
     ?assertEqual(graph:remove_edge(
                    #{a => #{c => {1000,500}},
                      b => #{c => {1,0},d => {1000,501}},
                      c => #{a => {500,1000},b => {0,1}},
                      d => #{b => {501,1000}}},
                    b, c),
                    #{a => #{c => {1000,500}},
                      b => #{d => {1000,501}},
                      c => #{a => {500,1000}},
                      d => #{b => {501,1000}}}),
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


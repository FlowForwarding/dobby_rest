-module(dbyr_search_test).

-include_lib("eunit/include/eunit.hrl").

-compiler(export_all).

dbyr_search_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {foreach,
       fun each_setup/0,
       [
         {"no matches", fun search1/0}
        ,{"metadata match", fun search2/0}
        ,{"link match", fun search3/0}
        ,{"path match A", fun search4a/0}
        ,{"path match B", fun search4b/0}
        ,{"path match C", fun search4c/0}
        ,{"path match D", fun search4d/0}
        ,{"path match E", fun search4e/0}
        ,{"path match F", fun search4f/0}
        ,{"path match G", fun search4g/0}
       ]
     }
    }.

setup() ->
    ok.

cleanup(ok) ->
    ok.

each_setup() ->
    ok.

search1() ->
    % no filters
    SearchFn = dbyr_search:subgraph(search_options()),
    {continue, _} = SearchFn(<<"id2">>,
                             #{},
                             [{<<"id1">>, #{}, #{}}], new_state()).

search2() ->
    % metadata match
    SearchOptions = maps:put(match_metadata,
                                [{<<"type">>, [<<"matchme">>]}],
                                    search_options()),
    SearchFn = dbyr_search:subgraph(SearchOptions),
    {skip, _} = SearchFn(<<"id2">>,
                         #{},
                         [{<<"id1">>, #{}, #{}}], new_state()),
    {skip, _} = SearchFn(<<"id2">>,
                         metadata_type(<<"dontmatchme">>),
                         [{<<"id1">>, #{}, #{}}], new_state()),
    {continue, _} = SearchFn(<<"id2">>,
                             metadata_type(<<"matchme">>),
                             [{<<"id1">>, #{}, #{}}], new_state()).

search3() ->
    % link metadata match
    SearchOptions = maps:put(match_links,
                                [{<<"type">>, [<<"matchme">>]}],
                                    search_options()),
    SearchFn = dbyr_search:subgraph(SearchOptions),
    {skip, _} = SearchFn(<<"id2">>, #{},
                         [{<<"id1">>, #{}, #{}}], new_state()),
    {continue, _} = SearchFn(<<"id2">>, #{},
                             [{<<"id1">>, #{}, metadata_type(<<"matchme">>)}],
                             new_state()).

search4a() ->
    % find path
    SearchFn = path_search_fn(),
    {_, {IdentifiersA, LinksA}} =
        SearchFn(<<"id1">>, metadata_type(<<"first">>),
                 [
                  {<<"id2">>, metadata_type(<<"second">>), #{}},
                  {<<"id3">>, metadata_type(<<"third">>), #{}}
                 ], new_state()),
    ?assertEqual(2, dict:size(LinksA)),
    ?assertEqual(3, dict:size(IdentifiersA)).

search4b() ->
    % find path (reverse)
    SearchFn = path_search_fn(),
    {_, {IdentifiersB, LinksB}} =
        SearchFn(<<"id3">>, metadata_type(<<"third">>),
                 [
                  {<<"id2">>, metadata_type(<<"second">>), #{}},
                  {<<"id1">>, metadata_type(<<"first">>), #{}}
                 ], new_state()),
    ?assertEqual(2, dict:size(LinksB)),
    ?assertEqual(3, dict:size(IdentifiersB)).

search4c() ->
    % do not find path
    SearchFn = path_search_fn(),
    {_, {IdentifiersC, LinksC}} =
        SearchFn(<<"id1">>, metadata_type(<<"one">>),
                 [
                  {<<"id2">>, metadata_type(<<"dos">>), #{}},
                  {<<"id3">>, metadata_type(<<"three">>), #{}}
                 ], new_state()),
    ?assertEqual(0, dict:size(LinksC)),
    ?assertEqual(0, dict:size(IdentifiersC)).

search4d() ->
    % find path if not at starting point
    SearchFn = path_search_fn(),
    {_, {IdentifiersA, LinksA}} =
        SearchFn(<<"id1">>, metadata_type(<<"nomatch">>),
                 [
                  {<<"id2">>, metadata_type(<<"first">>), #{}},
                  {<<"id3">>, metadata_type(<<"second">>), #{}},
                  {<<"id4">>, metadata_type(<<"third">>), #{}}
                 ], new_state()),
    ?assertEqual(2, dict:size(LinksA)),
    ?assertEqual(3, dict:size(IdentifiersA)).

search4e() ->
    % find path if not at starting point (reverse)
    SearchFn = path_search_fn(),
    {_, {IdentifiersB, LinksB}} =
        SearchFn(<<"id3">>, metadata_type(<<"nomatch">>),
                 [
                  {<<"id4">>, metadata_type(<<"third">>), #{}},
                  {<<"id2">>, metadata_type(<<"second">>), #{}},
                  {<<"id1">>, metadata_type(<<"first">>), #{}}
                 ], new_state()),
    ?assertEqual(2, dict:size(LinksB)),
    ?assertEqual(3, dict:size(IdentifiersB)).

search4f() ->
    % find path duplicated, no overlap
    SearchFn = path_search_fn(),
    {_, {IdentifiersA, LinksA}} =
        SearchFn(<<"id1">>, metadata_type(<<"nomatch">>),
                 [
                  {<<"id2">>, metadata_type(<<"first">>), #{}},
                  {<<"id3">>, metadata_type(<<"second">>), #{}},
                  {<<"id4">>, metadata_type(<<"third">>), #{}},
                  {<<"id5">>, metadata_type(<<"first">>), #{}},
                  {<<"id6">>, metadata_type(<<"second">>), #{}},
                  {<<"id7">>, metadata_type(<<"third">>), #{}}
                 ], new_state()),
    ?assertEqual(4, dict:size(LinksA)),
    ?assertEqual(6, dict:size(IdentifiersA)).

search4g() ->
    % find path overlapping
    SearchOptions = maps:put(match_path,
        [
         [
          #{element => identifier, match_metadata => [{<<"type">>, [<<"first">>]}]},
          #{element => identifier, match_metadata => [{<<"type">>, [<<"second">>]}]},
          #{element => identifier, match_metadata => [{<<"type">>, [<<"third">>]}]},
          #{element => identifier, match_metadata => [{<<"type">>, [<<"first">>]}]}
         ]
        ], search_options()),
    SearchFn = dbyr_search:subgraph(SearchOptions),
    {_, {IdentifiersA, LinksA}} =
        SearchFn(<<"id1">>, metadata_type(<<"nomatch">>),
                 [
                  {<<"id2">>, metadata_type(<<"first">>), #{}},
                  {<<"id3">>, metadata_type(<<"second">>), #{}},
                  {<<"id4">>, metadata_type(<<"third">>), #{}},
                  {<<"id5">>, metadata_type(<<"first">>), #{}},
                  {<<"id6">>, metadata_type(<<"second">>), #{}},
                  {<<"id7">>, metadata_type(<<"third">>), #{}},
                  {<<"id8">>, metadata_type(<<"first">>), #{}}
                 ], new_state()),
    ?assertEqual(6, dict:size(LinksA)),
    ?assertEqual(7, dict:size(IdentifiersA)).

% helpers

metadata_type(V) ->
    #{<<"type">> => #{value => V}}.

new_state() ->
    {dict:new(), dict:new()}.

search_options() ->
    #{max_depth => 0,
      traversal => depth,
      max_size => 100,
      match_metadata => any,
      match_links => any,
      match_path => [],
      results_filter => all,
      match_terminal => none}.

path_search_fn() ->
    SearchOptions = maps:put(match_path,
        [
         [
          #{element => identifier, match_metadata => [{<<"type">>, [<<"first">>]}]},
          #{element => identifier, match_metadata => [{<<"type">>, [<<"second">>]}]},
          #{element => identifier, match_metadata => [{<<"type">>, [<<"third">>]}]}
         ]
        ], search_options()),
    dbyr_search:subgraph(SearchOptions).

tr() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(dbyr_search, []).

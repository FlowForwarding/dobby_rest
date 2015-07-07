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
        ,{"path match", fun search4/0}
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

search4() ->
    % match path
    SearchOptions = maps:put(match_path,
        [
         [
          #{element => identifier, match_metadata => [{<<"type">>, [<<"first">>]}]},
          #{element => identifier, match_metadata => [{<<"type">>, [<<"second">>]}]},
          #{element => identifier, match_metadata => [{<<"type">>, [<<"third">>]}]}
         ]
        ], search_options()),
    SearchFn = dbyr_search:subgraph(SearchOptions),

    % find path
    {_, {IdentifiersA, LinksA}} =
        SearchFn(<<"id1">>, metadata_type(<<"first">>),
                 [
                  {<<"id2">>, metadata_type(<<"second">>), #{}},
                  {<<"id3">>, metadata_type(<<"third">>), #{}}
                 ], new_state()),
    ?assertEqual(2, dict:size(LinksA)),
    ?assertEqual(3, dict:size(IdentifiersA)),

    % find path (reverse)
    {_, {IdentifiersB, LinksB}} =
        SearchFn(<<"id3">>, metadata_type(<<"third">>),
                 [
                  {<<"id2">>, metadata_type(<<"second">>), #{}},
                  {<<"id1">>, metadata_type(<<"first">>), #{}}
                 ], new_state()),
    ?assertEqual(2, dict:size(LinksB)),
    ?assertEqual(3, dict:size(IdentifiersB)),

    % do not find path
    {_, {IdentifiersC, LinksC}} =
        SearchFn(<<"id1">>, metadata_type(<<"one">>),
                 [
                  {<<"id2">>, metadata_type(<<"dos">>), #{}},
                  {<<"id3">>, metadata_type(<<"three">>), #{}}
                 ], new_state()),
    ?assertEqual(0, dict:size(LinksC)),
    ?assertEqual(0, dict:size(IdentifiersC)).

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

tr() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(dbyr_search, []).

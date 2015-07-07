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
         {"no filters", fun search1/0}
        ,{"metadata filter", fun search2/0}
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
    % metadata filter
    SearchOptions = maps:put(match_metadata,
                                [{<<"type">>, [<<"matchme">>]}],
                                    search_options()),
    SearchFn = dbyr_search:subgraph(SearchOptions),
    {skip, _} = SearchFn(<<"id2">>,
                         #{},
                         [{<<"id1">>, #{}, #{}}], new_state()),
    {skip, _} = SearchFn(<<"id2">>,
                         #{<<"type">> => #{value => <<"dontmatchme">>}},
                         [{<<"id1">>, #{}, #{}}], new_state()),
    {continue, _} = SearchFn(<<"id2">>,
                             #{<<"type">> => #{value => <<"matchme">>}},
                             [{<<"id1">>, #{}, #{}}], new_state()).

% helpers

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

-module(dbyr_identifier_search_handler).

-include("dbyr_logger.hrl").

-export([init/3,
         rest_init/2,
         resource_exists/2,
         previously_existed/2,
         allow_missing_post/2,
         known_methods/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         handle_json/2
        ]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req0, _Opts) ->
    {Identifier, Req1} = cowboy_req:binding(identifier_val, Req0),
    {ok, Req1, #{identifier => Identifier,
                 exists => false,
                 metadata => #{}}}.

previously_existed(Req, State) -> 
    {false, Req, State}.

allow_missing_post(Req, State) ->
    {true, Req, State}.

known_methods(Req, State) -> 
    {[<<"GET">>,<<"POST">>,<<"DELETE">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_json}], Req, State}.

resource_exists(Req0, #{identifier := Identifier} = State) ->
    case dbyr_identifier:get_metadata(Identifier) of
        {error, Reason} -> 
            {ok, Req1} = cowboy_req:reply(500, [], stringify(Reason), Req0),
            {stop, Req1, State};
        [] ->
            {false, Req0, State};
        Metadata ->
            {true, Req0, State#{exists := true, metadata := Metadata}}
    end.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, handle_json}], Req, State}.

handle_json(Req0, State) ->
    {Method, Req1} = cowboy_req:method(Req0),
    handle_json_method(Req1, State, Method).

handle_json_method(Req0, #{identifier := Identifier} = State, <<"POST">>) ->
    case cowboy_req:body(Req0) of
        {ok, <<>>, Req1} ->
            {false, Req1, State};
        {ok, Body, Req1} ->
            Options = search_options(jiffy:decode(Body)),
            case dbyr_search:search(Identifier, Options) of
                {error, Reason} ->
                    ?ERROR("search error ~p~n", [Reason]),
                    {false, Req1, State};
                Results ->
                    {true,
                     cowboy_req:set_resp_body(jiffy:encode(Results), Req1),
                     State}
            end
    end.

stringify(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

search_options({Options}) ->
    search_options(Options, #{max_depth => 0,
                              traversal => depth,
                              max_size => 100,
                              match_metadata => any,
                              match_links => any,
                              results_filter => all,
                              match_terminal => none}).

search_options([], Options) ->
    Options;
search_options([{<<"max_depth">>, MaxDepth} | Rest], Options) ->
    search_options(Rest, Options#{max_depth := MaxDepth});
search_options([{<<"traversal">>, <<"depth">>} | Rest], Options) ->
    search_options(Rest, Options#{traversal := depth});
search_options([{<<"traversal">>, <<"breadth">>} | Rest], Options) ->
    search_options(Rest, Options#{traversal := breadth});
search_options([{<<"max_size">>, MaxSize} | Rest], Options) ->
    search_options(Rest, Options#{max_size := MaxSize});
search_options([{<<"match_metadata">>, "all"} | Rest], Options) ->
    search_options(Rest, Options#{match_metadata := all});
search_options([{<<"match_metadata">>, {Match}} | Rest], Options) ->
    search_options(Rest, Options#{match_metadata := matches_list(Match)});
search_options([{<<"match_links">>, <<"all">>} | Rest], Options) ->
    search_options(Rest, Options#{match_links := all});
search_options([{<<"match_links">>, {Match}} | Rest], Options) ->
    search_options(Rest, Options#{match_links := matches_list(Match)});
search_options([{<<"results_filter">>, <<"all">>} | Rest], Options) ->
    search_options(Rest, Options#{results_filter := all});
search_options([{<<"results_filter">>, Filter} | Rest], Options) ->
    search_options(Rest, Options#{results_filter := Filter});
search_options([{<<"match_terminal">>, <<"none">>} | Rest], Options) ->
    search_options(Rest, Options#{match_terminal := none});
search_options([{<<"match_terminal">>, {Match}} | Rest], Options) ->
    search_options(Rest, Options#{match_terminal := matches_list(Match)}).

matches_list(L) ->
    [match_list(E) || E <- L].

match_list(M = {_, L}) when is_list(L) ->
    M;
match_list({K, B}) ->
    {K, [B]}.

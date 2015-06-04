-module(dbyr_link_metadata_handler).

-include("dbyr_logger.hrl").

-export([init/3,
         rest_init/2,
         resource_exists/2,
         previously_existed/2,
         allow_missing_post/2,
         known_methods/2,
         allowed_methods/2,
         options/2,
         content_types_provided/2,
         content_types_accepted/2,
         delete_resource/2,
         handle_json/2
        ]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req0, _Opts) ->
    {Identifier1, Req1} = cowboy_req:binding(identifier1, Req0),
    {Identifier2, Req2} = cowboy_req:binding(identifier2, Req1),
    {Property, Req2} = cowboy_req:binding(property, Req1),
    {ok, Req2, #{link => {Identifier1, Identifier2},
                 property => Property,
                 exists => false,
                 value => null}}.

previously_existed(Req, State) -> 
    {false, Req, State}.

allow_missing_post(Req, State) ->
    {true, Req, State}.

known_methods(Req, State) -> 
    {[<<"OPTIONS">>,<<"GET">>,<<"POST">>,<<"DELETE">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>,<<"GET">>,<<"POST">>,<<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_json}], Req, State}.

options(Req0, State) ->
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>,
                                                    <<"content-type">>, Req0),
    {ok, Req1, State}.

resource_exists(Req0, #{link := Link,
                        property := Property} = State) ->
    case dbyr_link:get_metadata(Link) of
        {error, Reason} -> 
            {ok, Req1} = cowboy_req:reply(500, [], stringify(Reason), Req0),
            {stop, Req1, State};
        [] ->
            {false, Req0, State};
        Metadata ->
            case maps:find(Property, Metadata) of
                error ->
                    {false, Req0, State};
                {ok, Value} ->
                    {true, Req0, State#{exists := true, value := Value}}
            end
    end.

delete_resource(Req0, #{link := Link, property := Property} = State) ->
    case dbyr_link:delete_metadata(Link, Property) of
        {error, Reason} ->
            {ok, Req1} = cowboy_req:reply(500, [], stringify(Reason), Req0),
            {stop, Req1, State};
        ok ->
            Req1 = set_cross_domain(Req0),
            {true, Req1, State}
    end.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, handle_json}], Req, State}.

handle_json(Req0, State) ->
    {Method, Req1} = cowboy_req:method(Req0),
    handle_json_method(Req1, State, Method).

handle_json_method(Req, #{exists := false} = State, <<"GET">>) ->
    {false, Req, State};
handle_json_method(Req0, #{exists := true,
                         value := Value} = State, <<"GET">>) ->
    Req1 = set_cross_domain(Req0),
    {dbyr_encode:metadata_to_json(Value), Req1, State};
handle_json_method(Req0, #{link := Link,
                           property := Property} = State, <<"POST">>) ->
    {Value, Req1} = case cowboy_req:body(Req0) of
        {ok, <<>>, R1} ->
            {[], R1};
        {ok, Body, R1} ->
            M = dbyr_metadata:value_to_term(jiffy:decode(Body)),
            {M, R1}
    end,
    case dbyr_link:publish(Link, [{Property, Value}]) of
        ok ->
            Req2 = set_cross_domain(Req1),
            Req3 = cowboy_req:set_resp_body(<<"true">>, Req2),
            {{true, dbyr_link:to_resource(Link)}, Req3, State};
        {error, Reason} ->
            ?ERROR("publish error: ~p~n", [Reason]),
            Req2 = set_cross_domain(Req1),
            {false, Req2, State}
    end.

stringify(Reason) ->
    io_lib:format("~p", [Reason]).

set_cross_domain(Req) ->
    cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req).

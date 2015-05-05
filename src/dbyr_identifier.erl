-module(dbyr_identifier).

-define(REST_PUBLISHER, <<"dobby_rest">>).

-export([to_json/2,
         to_term/1,
         to_resource/1,
         get_metadata/1,
         delete/1,
         publish/2,
         search/2]).

to_json(Identifier, Metadata) ->
    % return json encoded identifier with metadata
    jiffy:encode(to_jiffy(Identifier, Metadata)).

to_resource(Identifier) ->
    % return resource URI for identifier
    list_to_binary([<<"/identifier/">>, Identifier]).

to_term({KeyValues}) ->
    [{Key, term_metadata(Value)} || {Key, Value} <- KeyValues].

get_metadata(Identifier) ->
    dby:identifier(Identifier).

delete(Identifier) ->
    dby:publish(?REST_PUBLISHER, {Identifier, delete}, [persistent]).

publish(Identifier, Metadata) ->
    dby:publish(?REST_PUBLISHER, {Identifier, Metadata}, [persistent]).

search(Identifier, Options) ->
    % XXX optimization - only install module once, not once per search
    {module, ?MODULE} = dby:install(?MODULE),
    case dby:search(subgraph(Options), dict:new(), Identifier,
                                [{loop, link} | search_options(Options)]) of
        {error, Reason} ->
            {error, Reason};
        GraphD ->
            [format_element(E) || {_, E} <- dict:to_list(GraphD)]
    end.

% ==============================================================================
% helper functions
% ==============================================================================

to_jiffy(Identifier, Metadata) ->
    {[
        {<<"identifier">>, Identifier},
        {<<"metdata">>, json_metadata(Metadata)}
    ]}.

to_jiffy(Id1, Id2, Metadata) ->
    {[
        {<<"link">>, to_link(Id1, Id2)},
        {<<"metadata">>, json_metadata(Metadata)}
    ]}.

to_link(Id1, Id2) ->
    list_to_binary([Id1, $/, Id2]).

json_metadata(true) ->
    true;
json_metadata(false) ->
    false;
json_metadata(null) ->
    null;
json_metadata(Data) when is_integer(Data) ->
    Data;
json_metadata(Data) when is_binary(Data) ->
    Data;
json_metadata(Data) when is_list(Data) ->
    lists:map(
        fun(Element) ->
            json_metadata(Element)
        end, Data);
json_metadata(Data) when is_map(Data) ->
    {
        maps:fold(
            fun(Key, Value, Acc) ->
                [{Key, json_metadata(Value)} | Acc]
            end, [], Data)
    }.

term_metadata(true) ->
    true;
term_metadata(false) ->
    false;
term_metadata(null) ->
    null;
term_metadata(Data) when is_integer(Data) ->
    Data;
term_metadata(Data) when is_binary(Data) ->
    Data;
term_metadata(Data) when is_list(Data) ->
    lists:map(
        fun(Element) ->
            term_metadata(Element)
        end, Data);
term_metadata({Map}) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            maps:put(Key, term_metadata(Value), Acc)
        end, #{}, Map).

search_options(Options) ->
    search_options(maps:to_list(Options), []).

search_options([], Acc) ->
    Acc;
search_options([{max_depth, MaxDepth} | Rest], Acc) ->
    search_options(Rest, [{max_depth, MaxDepth} | Acc]);
search_options([{traversal, depth} | Rest], Acc) ->
    search_options(Rest, [depth | Acc]);
search_options([{traversal, breadth} | Rest], Acc) ->
    search_options(Rest, [breadth | Acc]);
search_options([_ | Rest], Acc) ->
    search_options(Rest, Acc).

subgraph(_Options) ->
    % XXX use search options
    fun(Identifier, Metadata, [], Acc) ->
        {continue,
            dict:store(Identifier, {identifier, Identifier, Metadata}, Acc)};
       (Identifier, Metadata, [{Neighbor, _, LinkMetadata} | _], Acc) ->
        {continue, 
            dict:store(Identifier, {identifier, Identifier, Metadata},
                dict:store(normal_link(Identifier, Neighbor),
                            {link, Identifier, Neighbor, LinkMetadata},
                            Acc))}
    end.

normal_link(Id1, Id2) when Id1 < Id2 ->
    {Id1, Id2};
normal_link(Id1, Id2) ->
    {Id2, Id1}.

format_element({identifier, Identifier, Metadata}) ->
    to_jiffy(Identifier, Metadata);
format_element({link, Id1, Id2, Metadata}) ->
    to_jiffy(Id1, Id2, Metadata).

-module(dbyr_identifier).

-define(REST_PUBLISHER, <<"dobby_rest">>).

-export([to_json/2,
         to_term/1,
         to_resource/1,
         get_metadata/1,
         delete/1,
         publish/2]).

to_json(Identifier, Metadata) ->
    % return json encoded identifier with metadata
    jiffy:encode({[
        {<<"identifier">>, Identifier},
        {<<"metdata">>, json_metadata(Metadata)}
    ]}).

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

% ==============================================================================
% helper functions
% ==============================================================================

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

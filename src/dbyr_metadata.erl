-module(dbyr_metadata).

-export([to_term/1,
         value_to_term/1]).

to_term({KeyValues}) ->
    [{Key, term_metadata(Value)} || {Key, Value} <- KeyValues].

value_to_term(Value) ->
    term_metadata(Value).

% ==============================================================================
% helper functions
% ==============================================================================

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

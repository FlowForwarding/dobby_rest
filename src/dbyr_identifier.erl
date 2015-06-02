-module(dbyr_identifier).

-define(REST_PUBLISHER, <<"dobby_rest">>).

-export([to_json/2,
         metadata_to_json/1,
         to_term/1,
         value_to_term/1,
         to_resource/1,
         get_metadata/1,
         delete/1,
         delete_metadata/2,
         publish/2,
         search/2]).

to_json(Identifier, Metadata) ->
    % return json encoded identifier with metadata
    jiffy:encode(to_jiffy(Identifier, Metadata)).

metadata_to_json(Metadata) ->
    jiffy:encode(json_metadata(Metadata)).

to_resource(Identifier) ->
    % return resource URI for identifier
    iolist_to_binary([<<"/identifier/">>, uri_encode(Identifier)]).

to_term({KeyValues}) ->
    [{Key, term_metadata(Value)} || {Key, Value} <- KeyValues].

value_to_term(Value) ->
    term_metadata(Value).

get_metadata(Identifier) ->
    dby:identifier(Identifier).

delete(Identifier) ->
    dby:publish(?REST_PUBLISHER, {Identifier, delete}, [persistent]).

delete_metadata(Identifier, Property) ->
    dby:publish(?REST_PUBLISHER,
                    {Identifier, [{Property, delete}]}, [persistent]).

publish(Identifier, Metadata) ->
    dby:publish(?REST_PUBLISHER, {Identifier, Metadata}, [persistent]).

search(Identifier, Options) ->
    % XXX optimization - only install module once, not once per search
    {module, ?MODULE} = dby:install(?MODULE),
    case dby:search(subgraph(Options), {dict:new(), dict:new()}, Identifier,
                            [{loop, link} | dby_search_options(Options)]) of
        {error, Reason} ->
            {error, Reason};
        {Identifiers, Links} ->
            % XXX implement results_filter
            {[
                {<<"identifiers">>, 
                    [to_jiffy(I, M) ||
                                    {_, {I, M}} <- dict:to_list(Identifiers)]},
                {<<"links">>, 
                    [to_jiffy(I1, I2, L) ||
                                    {_, {I1, I2, L}} <- dict:to_list(Links)]}
            ]}
    end.

% ==============================================================================
% helper functions
% ==============================================================================

to_jiffy(Identifier, Metadata) ->
    {[
        {<<"identifier">>, uri_encode(Identifier)},
        {<<"metadata">>, json_metadata(Metadata)}
    ]}.

to_jiffy(Id1, Id2, Metadata) ->
    {[
        {<<"link">>, to_link(Id1, Id2)},
        {<<"metadata">>, json_metadata(Metadata)}
    ]}.

to_link(Id1, Id2) ->
    iolist_to_binary([uri_encode(Id1), $/, uri_encode(Id2)]).

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

dby_search_options(Options) ->
    dby_search_options(maps:to_list(Options), []).

dby_search_options([], Acc) ->
    Acc;
dby_search_options([{max_depth, MaxDepth} | Rest], Acc) ->
    dby_search_options(Rest, [{max_depth, MaxDepth} | Acc]);
dby_search_options([{traversal, depth} | Rest], Acc) ->
    dby_search_options(Rest, [depth | Acc]);
dby_search_options([{traversal, breadth} | Rest], Acc) ->
    dby_search_options(Rest, [breadth | Acc]);
dby_search_options([_ | Rest], Acc) ->
    dby_search_options(Rest, Acc).

subgraph(Options) ->
    % XXX implement max_size
    ControlFn = control_fn(Options),
    fun(Identifier, Metadata, [], {Identifiers, Links}) ->
        % always include the starting identifier
        {continue,
            {
                dict:store(Identifier, {Identifier, Metadata}, Identifiers),
                Links
            }
        };
       (Identifier, Metadata, [{Neighbor, _, LinkMetadata} | _],
                                                Acc = {Identifiers, Links}) ->
        Control = ControlFn(Metadata, LinkMetadata),
        {Control,
            case Control of
                skip ->
                    Acc;
                C when C == stop; C == continue ->
                    {
                        dict:store(Identifier,
                                    {Identifier, Metadata},
                                    Identifiers),
                        dict:store(normal_link(Identifier, Neighbor),
                                    {Identifier, Neighbor, LinkMetadata},
                                    Links)
                    }
            end
        }
    end.

control_fn(Options) ->
    MatchLinksFn = match_links_fn(Options),
    MatchMetaFn = match_meta_fn(Options),
    MatchTerminalFn = match_terminal_fn(Options),
    fun(Metadata, LinkMetadata) ->
        control_rank(
            MatchTerminalFn(Metadata),
            control_rank(
                MatchMetaFn(Metadata),
                control_rank(
                    MatchLinksFn(LinkMetadata),
                    continue)))
    end.

match_links_fn(#{match_links := any}) ->
    fun(_) -> continue end;
match_links_fn(#{match_links := Matches}) ->
    fun(LinkMetadata) ->
        case match_metadata(LinkMetadata, Matches) of
            true -> continue;
            false -> skip
        end
    end.

match_meta_fn(#{match_metadata := any}) ->
    fun(_) -> continue end;
match_meta_fn(#{match_metadata := Matches}) ->
    fun(Metadata) ->
        case match_metadata(Metadata, Matches) of
            true -> continue;
            false -> skip
        end
    end.

match_terminal_fn(#{match_terminal := none}) ->
    fun(_) -> continue end;
match_terminal_fn(#{match_terminal := Matches}) ->
    fun(Metadata) ->
        case match_metadata(Metadata, Matches) of
            true -> stop;
            false -> continue
        end
    end.

match_metadata(Metadata, Matches) ->
    lists:foldl(
        fun(Match, Acc) ->
            Acc andalso match_one(Match, Metadata)
        end, true, Matches).

match_one({Key, Value}, Metadata) ->
    case maps:find(Key, Metadata) of
        error -> false;
        {ok, Metadatum} ->
            Value == maps:get(value, Metadatum)
    end.

control_rank(_, stop) ->
    stop;
control_rank(stop, _) ->
    stop;
control_rank(_, skip) ->
    skip;
control_rank(skip, _) ->
    skip;
control_rank(continue, _) ->
    continue.

normal_link(Id1, Id2) when Id1 < Id2 ->
    {Id1, Id2};
normal_link(Id1, Id2) ->
    {Id2, Id1}.

uri_encode(B) when is_binary(B) ->
    list_to_binary(http_uri:encode(binary_to_list(B)));
uri_encode(S) ->
    http_uri:encode(S).

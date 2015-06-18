-module(dbyr_search).

-export([search/2]).

-ifdef(TEST).
-export([subgraph/1]).
-endif.

search(Identifier, Options) ->
    % XXX optimization - only install module once, not once per search
    {module, ?MODULE} = dby:install(?MODULE),
    % XXX force breadth search to work around bug in dobby with depth search.
    case dby:search(subgraph(Options), {dict:new(), dict:new()}, Identifier,
                            [breadth, {loop, link} | dby_search_options(Options)]) of
        {error, Reason} ->
            {error, Reason};
        {Identifiers, Links} ->
            {[
                {<<"identifiers">>, 
                    [dbyr_encode:to_jiffy(I, M) ||
                                    {_, {I, M}} <- dict:to_list(Identifiers)]},
                {<<"links">>, 
                    [dbyr_encode:to_jiffy({I1, I2}, L) ||
                                    {_, {I1, I2, L}} <- dict:to_list(Links)]}
            ]}
    end.

% ==============================================================================
% helper functions
% ==============================================================================

dby_search_options(Options) ->
    dby_search_options(maps:to_list(Options), []).

dby_search_options([], Acc) ->
    Acc;
dby_search_options([{max_depth, MaxDepth} | Rest], Acc) ->
    dby_search_options(Rest, [{max_depth, MaxDepth} | Acc]);
% XXX force breadth search to work about bug in dobby with depth search
% XXX see search/2 above.
% dby_search_options([{traversal, depth} | Rest], Acc) ->
%     dby_search_options(Rest, [depth | Acc]);
% dby_search_options([{traversal, breadth} | Rest], Acc) ->
%     dby_search_options(Rest, [breadth | Acc]);
dby_search_options([_ | Rest], Acc) ->
    dby_search_options(Rest, Acc).

subgraph(Options) ->
    % XXX implement max_size
    ControlFn = control_fn(Options),
    FilterFn = filter_fn(Options),
    fun(Identifier, Metadata, [], {Identifiers, Links}) ->
        % always include the starting identifier
        {continue,
            {
                dict:store(Identifier, {Identifier, FilterFn(Metadata)},
                                                                Identifiers),
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
                                    {Identifier, FilterFn(Metadata)},
                                    Identifiers),
                        dict:store(normal_link(Identifier, Neighbor),
                                    {Identifier, Neighbor,
                                                    FilterFn(LinkMetadata)},
                                    Links)
                    }
            end
        }
    end.

control_fn(Options) ->
    MatchLinksFn = match_links_fn(Options),
    % XXX results filter should be in dobby?
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

filter_fn(#{results_filter := All}) when All == all; All == [] ->
    fun(Metadata) ->
        Metadata
    end;
filter_fn(#{results_filter := AllowedKeys}) ->
    fun(Metadata) ->
        maps_with(AllowedKeys, Metadata)
    end.

maps_with(WithKeys, Map) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            case lists:member(Key, WithKeys) of
                false ->
                    Acc;
                true ->
                    maps:put(Key, Value, Acc)
            end
        end, #{}, Map).

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

match_one({Key, Values}, Metadata) ->
    case maps:find(Key, Metadata) of
        error -> false;
        {ok, Metadatum} ->
            lists:member(maps:get(value, Metadatum), Values)
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

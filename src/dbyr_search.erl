-module(dbyr_search).

-export([search/2]).

-ifdef(TEST).
-export([subgraph/1,
        normalize_path/1,
        found_path/3,
        match_paths/4]).
-endif.

search(Identifier, Options) ->
    % XXX optimization - only install module once, not once per search
    {module, ?MODULE} = dby:install(?MODULE),
    % XXX force breadth search to work around bug in dobby with depth search.
    case dby:search(subgraph(Options), {dict:new(), dict:new()}, Identifier,
                            [{loop, link} | dby_search_options(Options)]) of
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
  dby_search_options([{traversal, depth} | Rest], Acc) ->
      dby_search_options(Rest, [depth | Acc]);
  dby_search_options([{traversal, breadth} | Rest], Acc) ->
      dby_search_options(Rest, [breadth | Acc]);
dby_search_options([_ | Rest], Acc) ->
    dby_search_options(Rest, Acc).

subgraph(Options) ->
    % XXX implement max_size
    ControlFn = control_fn(Options),
    FilterFn = filter_fn(Options),
    PathFn = path_fn(Options, FilterFn),
    fun(Identifier, Metadata, [], {Identifiers, Links}) ->
        % always include the starting identifier
        {continue,
            {
                dict:store(Identifier, {Identifier, FilterFn(Metadata)},
                                                                Identifiers),
                Links
            }
        };
       (Identifier, Metadata, Path = [{_, _, LinkMetadata} | _], Acc) ->
        Control = ControlFn(Metadata, LinkMetadata),
        {Control,
            case Control of
                skip ->
                    Acc;
                C when C == stop; C == continue ->
                    PathFn(Identifier, Metadata, Path, Acc)
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

path_fn(#{match_path := []}, FilterFn) ->
    fun(Identifier, Metadata, [{Neighbor, _, LinkMetadata} | _],
                                                    {Identifiers, Links}) ->
        {
            dict:store(Identifier,
                        {Identifier, FilterFn(Metadata)},
                        Identifiers),
            dict:store(normal_link(Identifier, Neighbor),
                        {Identifier, Neighbor, FilterFn(LinkMetadata)},
                        Links)
        }
    end;
path_fn(#{match_path := MatchPaths}, FilterFn) ->
    NormalMatchPaths = reverse_paths(normalize_paths(MatchPaths)),
    fun(Identifier, Metadata, Path, Acc) ->
        FoundPath = found_path(Identifier, Metadata, Path),
        AccFn =
            fun(MatchingPath, {Identifiers, Links}) ->
                {
                    dict_store_identifiers(Identifiers, MatchingPath, FilterFn),
                    dict_store_links(Links, MatchingPath, FilterFn)
                }
            end,
        match_paths(NormalMatchPaths, FoundPath, AccFn, Acc)
    end.

% add the identifiers from the path to the dict
dict_store_identifiers(Identifiers, [], _) ->
    Identifiers;
dict_store_identifiers(Identifiers, [#{element := link} | Path], FilterFn) ->
    dict_store_identifiers(Identifiers, Path, FilterFn);
dict_store_identifiers(Identifiers, [#{element := identifier,
                                       id := Identifier,
                                       metadata := Metadata} | Path],
                                    FilterFn) ->
    dict_store_identifiers(
        dict:store(Identifier, {Identifier, FilterFn(Metadata)}, Identifiers),
        Path, FilterFn).

% add the links from the path to the dict
dict_store_links(Links, [], _) ->
    % nothing to add
    Links;
dict_store_links(Links, [#{}], _) ->
    % only the trailing identifier remains
    Links;
dict_store_links(Links,
                 [#{id := Identifier}, #{metadata := LinkMetadata} | Path],
                 FilterFn) ->
    [#{id := Neighbor} | _] = Path,
    dict_store_links(
        dict:store(normal_link(Identifier, Neighbor),
                   {Identifier, Neighbor, FilterFn(LinkMetadata)},
                   Links),
        Path, FilterFn).

% add missing links/identifiers
normalize_paths(MatchPaths) ->
    [normalize_path(MatchPath) || MatchPath <- MatchPaths].

normalize_path(MatchPath) ->
    normalize_path(MatchPath, []).

normalize_path([], Acc = [#{element := link} | _]) ->
    % ends in a link, add trailing identifier
    normalize_path([], [wild_identifier() | Acc]);
normalize_path([], Acc) ->
    % done - the result is backwards but we need match against it
    % in both directions anyway so there's no need to get it
    % in the correct direction here.
    Acc;
normalize_path(MatchPath = [#{element := link} | _], []) ->
    % starts with a link, add initial identifier
    normalize_path(MatchPath, [wild_identifier()]);
normalize_path(MatchPath = [#{element := link} | _],
               Acc = [#{element := link} | _]) ->
    % insert an identifier between two links
    normalize_path(MatchPath, [wild_identifier() | Acc]);
normalize_path(MatchPath = [#{element := identifier} | _],
               Acc = [#{element := identifier} | _]) ->
    % insert a link between two identifiers
    normalize_path(MatchPath, [wild_link() | Acc]);
normalize_path([Element | Rest], Acc) ->
    % add to normalized path
        normalize_path(Rest, [Element | Acc]).

% make reverse paths for all the paths.
% return the original list plus all the reversed paths.
reverse_paths(Paths) ->
    [lists:reverse(Path) || Path <- Paths] ++ Paths.

wild_identifier() ->
    #{element => identifier, match_metadata => any}.

wild_link() ->
    #{element => link, match_metadata => any}.

% create path structure from search fun args to match against.
% the first element of the list is the starting point.
% match_path assumes this to anchor the search at the starting point.
found_path(Identifier, Metadata, Path) ->
    found_path(Path, [fp_identifier(Identifier, Metadata)]).

found_path([], Path) ->
    Path;
found_path([{Identifier, Metadata, LinkMetadata} | Rest], Path) ->
    found_path(Rest,
        [fp_identifier(Identifier, Metadata), fp_link(LinkMetadata) | Path]).

fp_link(Metadata) ->
    #{element => link, metadata => Metadata}.

fp_identifier(Identifier, Metadata) ->
    #{element => identifier, id => Identifier, metadata => Metadata}.

% look for any of the paths in MatchPaths in FoundPaths
match_paths(MatchPaths, FoundPath, AccFn, Acc0) ->
    lists:foldl(
        fun(MatchPath, Acc) ->
            AccFn(match_path(MatchPath, FoundPath, []), Acc)
        end, Acc0, MatchPaths).

% XXX want to anchor the path match on the starting point.
% but once we find that match in the path, should we also
% search the rest of the path for additional matches?  This would
% allow you to start at one switch and search the entire network
% for connected switches.  Without the additional matches, the
% path match stops after finding the neighbor switch.

% match the found path against the pattern.
% returns []  if no match, or the matching path if it does match.
match_path([], _, Matched) ->
    % path matched pattern, return match
    Matched;
match_path(MatchPath, FoundPath, _)
                                when length(MatchPath) > length(FoundPath) ->
    % found path is shorter than the pattern, no match
    [];
match_path([#{match_metadata := any} | MatchPathRest],
           [FoundElement | FoundPathRest],
           Matched) ->
    match_path(MatchPathRest, FoundPathRest, [FoundElement | Matched]);
match_path([#{match_metadata := MatchMetadata} | MatchPathRest],
           [FoundElement = #{metadata := FoundMetadata} | FoundPathRest],
           Matched) ->
    % match the metadata
    case match_metadata(FoundMetadata, MatchMetadata) of
        true ->
            % matches
            match_path(MatchPathRest, FoundPathRest, [FoundElement | Matched]);
        false ->
            []
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

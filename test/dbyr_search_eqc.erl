-module(dbyr_search_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

% ------------------------------------------------------------------------------
% Properties
% ------------------------------------------------------------------------------

% given a path with a mix of links and identifiers, the normlized
% path has identifiers as the first and last elements and links
% between all the identifiers in between.
prop_normalize_path() ->
    ?FORALL(
        Path,
        gen_match_path(),
        collect(length(Path),
            is_normal_path(dbyr_search:normalize_path(Path)))).

% given a match path and a found path, found paths matching the match are
% at least as long as the match path.
prop_match_path() ->
    numtests(500, ?FORALL(
        {MatchPath, FoundPath},
        {gen_match_path(), gen_found_path()},
        begin
            NormalMatchPath = dbyr_search:normalize_path(MatchPath),
            [Result] = dbyr_search:match_paths(
                [NormalMatchPath],
                FoundPath,
                fun(Path, Acc) -> [Path | Acc] end,
                []),
            Length = length(Result),
            collect(Length,
                case Length of
                    0 ->
                        true;
                    Length ->
                        Length =:= length(NormalMatchPath)
                end
            )
        end)).
            
% ------------------------------------------------------------------------------
% Generators
% ------------------------------------------------------------------------------
gen_match_path() ->
    non_empty(list(gen_match_element())).

gen_match_element() ->
    oneof([#{element => identifier,
             id => binary(),
             match_metadata => gen_match_metadata()},
           #{element => link,
             match_metadata => gen_match_metadata()}]).

gen_match_metadata() ->
    [{<<"key1">>, oneof([[<<"A">>], [<<"B">>]])}].

gen_found_path() ->
    ?LET(
        P,
        non_empty(list({binary(), gen_found_metadata(), gen_found_metadata()})),
        begin
            [{Identifier, Metadata, _} | Path] = P,
            dbyr_search:found_path(Identifier, Metadata, Path)
        end
    ).

gen_found_metadata() ->
    #{<<"key1">> => #{value => oneof([<<"A">>, <<"B">>])}}.

% ------------------------------------------------------------------------------
% Comparisons
% ------------------------------------------------------------------------------

is_normal_path([#{element := identifier}]) ->
    true;
is_normal_path([#{element := identifier}, #{element := link} | Path]) ->
    is_normal_path(Path).

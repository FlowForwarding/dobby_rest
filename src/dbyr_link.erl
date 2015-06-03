-module(dbyr_link).

-define(REST_PUBLISHER, <<"dobby_rest">>).

-export([to_resource/1,
         get_metadata/1,
         delete/1,
         delete_metadata/2,
         publish/2]).

to_resource({Identifier1, Identifier2}) ->
    % return resource URI for identifier
    iolist_to_binary([<<"/link/">>,
                      dbyr_encode:uri_encode(Identifier1), $/, 
                      dbyr_encode:uri_encode(Identifier2)]).

get_metadata({Identifier1, Identifier2}) ->
    % XXX implement
    dby:identifier(Identifier1),
    dby:identifier(Identifier2).

delete({Identifier1, Identifier2}) ->
    dby:publish(?REST_PUBLISHER, {Identifier1, Identifier2, delete},
                                                                [persistent]).

delete_metadata({Identifier1, Identifier2}, Property) ->
    dby:publish(?REST_PUBLISHER,
                    {{Identifier1, Identifier2}, [{Property, delete}]},
                                                                [persistent]).

publish({Identifier1, Identifier2}, Metadata) ->
    dby:publish(?REST_PUBLISHER, {Identifier1, Identifier2, Metadata},
                                                                [persistent]).

% ==============================================================================
% helper functions
% ==============================================================================

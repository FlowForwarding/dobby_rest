-module(dbyr_identifier).

-define(REST_PUBLISHER, <<"dobby_rest">>).

-export([to_resource/1,
         get_metadata/1,
         delete/1,
         delete_metadata/2,
         publish/2]).

to_resource(Identifier) ->
    % return resource URI for identifier
    iolist_to_binary([<<"/identifier/">>, dbyr_encode:uri_encode(Identifier)]).

get_metadata(Identifier) ->
    dby:identifier(Identifier).

delete(Identifier) ->
    dby:publish(?REST_PUBLISHER, {Identifier, delete}, [persistent]).

delete_metadata(Identifier, Property) ->
    dby:publish(?REST_PUBLISHER,
                    {Identifier, [{Property, delete}]}, [persistent]).

publish(Identifier, Metadata) ->
    dby:publish(?REST_PUBLISHER, {Identifier, Metadata}, [persistent]).

% ==============================================================================
% helper functions
% ==============================================================================

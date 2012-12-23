-module(relatio_store).
-include_lib("relatio/src/relatio.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([put_node_set/1,
         get_node_set/1]).

to_id(Rec) ->
    erlang:element(2, Rec).

maybe_to_id(undefined) -> undefined;
maybe_to_id(Rec)       -> to_id(Rec).


maybe_element(_N, undefined) -> undefined;
maybe_element( N, Rec)       -> element(N, Rec).


-spec put_node_set(Value) -> Id when
    Value :: relatio_type:node_set_value(),
    Id :: relatio_type:node_set_id().

put_node_set(Value) ->
    to_id(relatio_db:write(#r_node_set{value = Value})).


%% Returns `undefined', if nothing was found.
-spec get_node_set(Id) -> Value | undefined when
    Value :: relatio_type:node_set_value(),
    Id :: relatio_type:node_set_id().

get_node_set(Id) ->
    maybe_element(#r_node_set.value, relatio_db:maybe_lookup(r_node_set, Id)).


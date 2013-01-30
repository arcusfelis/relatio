-module(relatio_locator).
-export([locate_world_xref_server/0, locate_world_inferno_server/0]).

locate_world_xref_server() ->
    whereis(relatio_world_xref).

locate_world_inferno_server() ->
    %% See `relatio_sup' for initialization.
    whereis(inferno_server).

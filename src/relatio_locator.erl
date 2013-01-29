-module(relatio_locator).
-export([locate_world_xref_server/0, locate_world_inferno_server/0]).

locate_world_xref_server() ->
    case whereis(relatio_world) of
        undefined ->
            {ok, Xref} = xref:start(relatio_world),
            link(Xref),
            Info = locate_world_inferno_server(),
            App2Dirs = inferno_server:application_directories(Info),
            io:format(user, "App2Dirs: ~p~n", [App2Dirs]),
            [xref:add_application(Xref, Dir, [{name, AppName}])
             || {AppName, [Dir|_]} <- App2Dirs],
            unlink(Xref),
            Xref;
        Xref ->
            Xref
    end.

locate_world_inferno_server() ->
    %% See `relatio_sup' for initialization.
    whereis(inferno_server).

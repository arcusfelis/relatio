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
    case whereis(inferno_server) of
        undefined ->
            {ok, Info} = inferno_server:start([]),
            link(Info),
            {ok, B} = file:read_file("relatio.config"),
            S = binary_to_list(B),
            {ok, Tokens, _} = erl_scan:string(S),
            {ok, [Form]} = erl_parse:parse_exprs(Tokens),
            {value, Value, _} = erl_eval:expr(Form, []),
            io:format(user, "Config: ~p~n", [Value]),
            [handle_config_command(X, Info) || X <- Value],
            erlang:register(inferno_server, Info),
            unlink(Info),
            Info;
        Info ->
            Info
    end.

handle_config_command({application, Name, Dir}, Info) ->
    inferno_server:add_application(Info, Name, Dir).

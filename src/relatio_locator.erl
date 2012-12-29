-module(relatio_locator).
-export([locate_world_xref_server/0, locate_world_inferno_server/0]).

locate_world_xref_server() ->
    %% TODO: rewrite this function completely.
    Dir1 = "/home/user/erlang/esl/ejabberd/apps/ejabberd",
    Dir2 = code:lib_dir(stdlib),
    Dir3 = code:lib_dir(kernel),
    Dir4 = code:lib_dir(mnesia),
    Dir5 = code:lib_dir(snmp),
    case whereis(relatio_world) of
        undefined ->
            {ok, Xref} = xref:start(relatio_world),
            xref:add_application(Xref, Dir1),
            xref:add_application(Xref, Dir2),
            xref:add_application(Xref, Dir3),
            xref:add_application(Xref, Dir4),
            xref:add_application(Xref, Dir5),
            Xref;
        Xref ->
            Xref
    end.

locate_world_inferno_server() ->
    %% TODO: rewrite this function completely.
    Dir1 = "/home/user/erlang/esl/ejabberd/apps/ejabberd",
    Dir2 = code:lib_dir(stdlib),
    Dir3 = code:lib_dir(kernel),
    Dir4 = code:lib_dir(mnesia),
    Dir5 = code:lib_dir(snmp),
    Dir2s = "/home/user/erlang/otp/lib/stdlib",
    Dir3s = "/home/user/erlang/otp/lib/kernel",
    Dir4s = "/home/user/erlang/otp/lib/mnesia",
    Dir5s = "/home/user/erlang/otp/lib/snmp",

    case whereis(inferno_server) of
        undefined ->
            {ok, Info} = inferno_server:start([]),
            inferno_server:add_application(Info, ejabberd, Dir1),

            inferno_server:add_application(Info, stdlib,   Dir2s),
            inferno_server:add_application(Info, kernel,   Dir3s),
            inferno_server:add_application(Info, mnesia,   Dir4s),
            inferno_server:add_application(Info, snmp,     Dir5s),

            inferno_server:add_application(Info, stdlib,   Dir2),
            inferno_server:add_application(Info, kernel,   Dir3),
            inferno_server:add_application(Info, mnesia,   Dir4),
            inferno_server:add_application(Info, snmp,     Dir5),
            erlang:register(inferno_server, Info),
            Info;
        Info ->
            Info
    end.


-module(relatio_data_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).
-compile({parse_transform, seqbind}).

%% ------------------------------------------------------------------
%% Callbacks
%% ------------------------------------------------------------------

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req@, State) ->
    {Path, Req@} = cowboy_http_req:path_info(Req@),
    case Path of
        [<<"world.gexf">>] ->
            {ok, Req@} = cowboy_http_req:reply(200, [], generate_world_xml(), Req@),
            {ok, Req@, State};
        [<<"v-e-m.gexf">>] ->
            {ok, Req@} = cowboy_http_req:reply(200, [], generate_xml(), Req@),
            {ok, Req@, State}
    end.

terminate(_Req, _State) ->
    ok.

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------
%   Dir = "/home/user/erlang/proper",
%   Dir = "/home/user/erlang/torrent/etorrent_core",
%   Dir = "/home/user/erlang/unicode/ux",
%   xref:add_application(Xref, code:lib_dir(gexf)),
%   xref:add_application(Xref, "/home/user/erlang/database/riak_core"),
%   xref:add_application(Xref, "/home/user/erlang/unicode/ux"),
%   xref:add_application(Xref, code:lib_dir(mnesia)),
%   xref:add_application(Xref, code:lib_dir(snmp)),

generate_xml() ->
    %% TODO: rewrite this function completely.
    Dir1 = "/home/user/erlang/esl/ejabberd/apps/ejabberd",
    Dir2 = code:lib_dir(stdlib),
    Dir3 = code:lib_dir(kernel),

    case whereis(inferno_server) of
        undefined ->
            {ok, Info} = inferno_server:start([]),
            inferno_server:add_application(Info, Dir1),
            inferno_server:add_application(Info, Dir2),
            inferno_server:add_application(Info, Dir3),
            erlang:register(inferno_server, self()),
            Info;
        Info ->
            Info
    end,
    link(Info),

    {ok, Xref} = xref:start([]),
%%  xref:add_module(Xref, Path),
    link(Xref),

    try
        gexf:to_string(gexf_xref:generate(Xref, Info))
    after 
        xref:stop(Xref),
        unlink(Info)
    end.


generate_world_xml() ->
    %% TODO: rewrite this function completely.
    Dir1 = "/home/user/erlang/esl/ejabberd/apps/ejabberd",
    Dir2 = code:lib_dir(stdlib),
    Dir3 = code:lib_dir(kernel),
    case whereis(relatio_world) of
        undefined ->
            {ok, Xref} = xref:start(relatio_world),
            xref:add_application(Xref, Dir1),
            xref:add_application(Xref, Dir2),
            xref:add_application(Xref, Dir3),
            Xref;
        Xref ->
            Xref
    end,
    link(Xref),
    try
        gexf:to_string(gexf_xref_world:generate(Xref))
    after 
        unlink(Xref)
    end.



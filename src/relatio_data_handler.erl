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
            {SetIdBin, Req@} = cowboy_http_req:qs_val(<<"node_set_id">>, Req@),
            SetId = list_to_integer(binary_to_list(SetIdBin)),
            Nodes = relatio_store:get_node_set(SetId),
            Reply = generate_xml(Nodes),
            {ok, Req@} = cowboy_http_req:reply(200, [], Reply, Req@),
            {ok, Req@, State};

        [<<"save_detalize">>] ->
            {PostVals, Req@} = cowboy_http_req:body_qs(Req@),
            io:format(user, "PostVals ~p~n", [PostVals]),
            NodesJSON = proplists:get_value(<<"nodes">>, PostVals),
            Nodes = jsx:decode(NodesJSON),
            io:format(user, "Nodes ~p~n", [Nodes]),
            %% Save a node set into database, get its id.
            SetId = relatio_store:put_node_set(Nodes),
            Reply = [{<<"node_set_id">>, SetId}],
            ReplyJSON = jsx:encode(Reply),
            {ok, Req@} = cowboy_http_req:reply(200, [], ReplyJSON, Req@),
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

generate_xml(Nodes) ->
    %% TODO: rewrite this function completely.
    Dir1 = "/home/user/erlang/esl/ejabberd/apps/ejabberd",
    Dir2 = code:lib_dir(stdlib),
    Dir3 = code:lib_dir(kernel),

    case whereis(inferno_server) of
        undefined ->
            {ok, Info} = inferno_server:start([]),
            inferno_server:add_application(Info, ejabberd, Dir1),
            inferno_server:add_application(Info, stdlib,   Dir2),
            inferno_server:add_application(Info, kernel,   Dir3),
            erlang:register(inferno_server, self()),
            Info;
        Info ->
            Info
    end,
    link(Info),

    {ok, Xref} = xref:start([]),
    add_targets(Nodes, Xref, Info),
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


add_targets(Nodes, Xref, Info) ->
    KVMaker = fun(Node) -> 
            Type = proplists:get_value(<<"type">>, Node),
            Name = proplists:get_value(<<"name">>, Node),
            {Type, list_to_atom(binary_to_list(Name))} %% {Key, Value} 
            %% Warning: list_to_atom is not save.
        end,
    %% Type2Names = [{<<"app">>, [<<"kernel">>]}, {<<"module">>, [<<"lists">>]}]
    Type2Names = lists2:map_group_with(KVMaker, Nodes),
    AppNames = proplists:get_value(<<"app">>, Type2Names, []),
    ModNames = proplists:get_value(<<"module">>, Type2Names, []),
    ModFileNames = inferno_server:
                   module_names_to_compiled_filenames(Info, ModNames),
    AppDirs = inferno_server:
              application_names_to_directories(Info, AppNames),
    io:format(user, "AppDirs: ~p~n", [AppDirs]),
    io:format(user, "ModFileNames: ~p~n", [ModFileNames]),
    [xref:add_application(Xref, AppDir)
     || {_AppName, AppDir} <- AppDirs, AppDir =/= undefined],
    [xref:add_module(Xref, ModFileName)
     || {_ModName, ModFileName} <- ModFileNames, ModFileName =/= undefined],
    ok.




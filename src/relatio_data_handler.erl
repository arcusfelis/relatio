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
        [<<"v-e-m.gexf">>] ->
            {ok, Req@} = cowboy_http_req:reply(200, [], generate_xml(), Req@),
            {ok, Req@, State}
    end.

terminate(_Req, _State) ->
    ok.

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

generate_xml() ->
    {ok, Xref} = xref:start(relatio_ex1),
%   xref:add_application(Xref, code:lib_dir(gexf)),
%   xref:add_application(Xref, "/home/user/erlang/database/riak_core"),
    xref:add_application(Xref, code:lib_dir(mnesia)),
%   xref:add_application(Xref, code:lib_dir(snmp)),
    try
        gexf:to_string(gexf_xref:'e-v-m'(Xref))
    after
        xref:stop(Xref)
    end.

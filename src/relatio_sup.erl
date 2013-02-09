-module(relatio_sup).
-behaviour(supervisor).

-export([start_link/0]). %% API.
-export([init/1]). %% supervisor.
%% private
-export([start_inferno/0, start_xref/0]).

-define(SUPERVISOR, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API.

-spec start_link() -> {ok, Pid::pid()}.
start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
    InfoSpec = {info, {?MODULE, start_inferno, []},
                      transient, 5000, worker, []},
    XrefSpec = {xref, {?MODULE, start_xref, []},
                      transient, 5000, worker, []},
	{ok, {{rest_for_one, 10, 10}, [InfoSpec, XrefSpec]}}.


start_inferno() ->
    {ok, Info} = inferno_server:start_link([]),
    {ok, B} = file:read_file("relatio.config"),
    S = binary_to_list(B),
    {ok, Tokens, _} = erl_scan:string(S),
    {ok, [Form]} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:expr(Form, []),
    io:format(user, "Config: ~p~n", [Value]),
    [handle_config_command(X, Info) || X <- Value],
    erlang:register(inferno_server, Info),
    {ok, Info}.


start_xref() ->
    {ok, Xref} = xref:start(relatio_world_xref),
    link(Xref),
    Info = relatio_locator:locate_world_inferno_server(),
    true = is_pid(Info),
    inferno_server:add_xref_handler(Info, Xref),
    {ok, Xref}.


handle_config_command({application, Name, Dir}, Info) ->
    inferno_server:add_application(Info, Name, Dir);
handle_config_command({application, Name, Dir, Params}, Info) ->
    inferno_server:add_application(Info, Name, Dir, Params).

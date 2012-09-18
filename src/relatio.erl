%% Feel free to use, reuse and abuse the code in this file.

-module(relatio).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(mimetypes),
	application:start(cowboy),
	application:start(jsx),
	application:start(bullet),
	application:start(sendfile),
	application:start(cowboy_static),
	application:start(relatio).


start(_Type, _Args) ->
    PrivDir = code:priv_dir(relatio),
    JsDir = abs_path(filename:join([PrivDir, "js"])),
    HtmlDir = abs_path(filename:join([PrivDir, "html"])),
    FontDir = abs_path(filename:join([PrivDir, "font"])),
    BulletDir = abs_path(code:priv_dir(bullet)),

	Dispatch = [
		{'_', [
			{[<<"stream">>], bullet_handler, 
                    [{handler, relatio_stream_handler}]},
			{[<<"data">>, '...'], relatio_data_handler, []},
			{[], relatio_default_handler, []},
            cowboy_static:rule([
                {dir, JsDir}, 
                {prefix, "js"}, 
                {sendfile, false}]),
            cowboy_static:rule([
                {dir, FontDir}, 
                {prefix, "font"}, 
                {sendfile, false}]),
            cowboy_static:rule([
                {dir, BulletDir}, 
                {prefix, "bullet"}, 
                {sendfile, false}]),
            cowboy_static:rule([
                {dir, HtmlDir}, 
                {prefix, ""}, 
                {sendfile, false}])
		]}
	],
	cowboy:start_listener(relatio_http, 100,
		cowboy_tcp_transport, [{port, 2080}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
%   cowboy:start_listener(https, 100,
%   	cowboy_ssl_transport, [
%   		{port, 1443}, {certfile, "priv/ssl/cert.pem"},
%   		{keyfile, "priv/ssl/key.pem"}, {password, "cowboy"}],
%   	cowboy_http_protocol, [{dispatch, Dispatch}]
%   ),
	relatio_sup:start_link().

stop(_State) ->
	ok.


%%
%% Private
%%

abs_path(Path) -> 
    filename:join(
        abs_path_(
            filename:split(
                filename:absname(Path)), [])).


abs_path_([".."|T], [_|Stack]) ->
    abs_path_(T, Stack);
abs_path_([H|T], Stack) ->
    abs_path_(T, [H|Stack]);
abs_path_([], Stack) ->
    lists:reverse(Stack).

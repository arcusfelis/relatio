-module(relatio_stream_handler).
-export([init/4, stream/3, info/3, terminate/2]).

-record(state, {
}).

init(_Transport, Req, _Opts, _Active) ->
    State = #state{ },
    {ok, Req, State}.


stream(Data, Req, State) ->
    {reply, Data, Req, State}.


info(Info, Req, State) ->
    {reply, Info, Req, State}.


terminate(_Req, _State) ->
    ok.


%%
%% API
%%


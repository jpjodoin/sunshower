-module(raincloud_handler).
-include_lib("kernel/include/logger.hrl").


-export([init/3, handle/2, terminate/3, start/0]).
-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).

%GET /submit/?idhash=0000000000&message=1xOuOYDYAQCgAi3dAP8AAgAAAAAAAg== HTTP/1.1 <-- Generate token if doesn't exist, else reuse existing

start() ->
    ?LOG_INFO("Test", []),
    Dispatch = cowboy_router:compile([
        {'_', [{"/", raincloud_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(raincloud_handler,
        [{port, 80}],
        #{env => #{dispatch => Dispatch}}
    ).

    
    init({tcp, http}, _Req, _Opts) ->
      {upgrade, protocol, cowboy_websocket}.
    
    
    handle(Req, State) ->
        ?LOG_DEBUG("Request not expected: ~p", [Req]),
        {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
        {ok, Req2, State}.
    
    
    websocket_init(_TransportName, Req, _Opts) ->
        ?LOG_DEBUG("init websocket"),
        {ok, Req, undefined_state}.
    
    websocket_handle({text, Msg}, Req, State) ->
        ?LOG_DEBUG("Got Data: ~p", [Msg]),
        {reply, {text, << "responding to ", Msg/binary >>}, Req, State, hibernate };
    
    
    websocket_handle(_Any, Req, State) ->
        {reply, {text, << "whut?">>}, Req, State, hibernate }.
    
    websocket_info({timeout, _Ref, Msg}, Req, State) ->
        {reply, {text, Msg}, Req, State};
    
    websocket_info(_Info, Req, State) ->
        ?LOG_DEBUG("websocket info"),
        {ok, Req, State, hibernate}.
    
    websocket_terminate(_Reason, _Req, _State) ->
        ok.
    
    terminate(_Reason, _Req, _State) ->
        ok.
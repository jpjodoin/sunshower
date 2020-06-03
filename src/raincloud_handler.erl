-module(raincloud_handler).
-include_lib("kernel/include/logger.hrl").
-include("type.hrl").

-export([init/2, terminate/3, start/0]).
-export([
    websocket_init/1, websocket_handle/2,
    websocket_info/2
]).

%GET /submit/?idhash=0000000000&message=1xOuOYDYAQCgAi3dAP8AAgAAAAAAAg== HTTP/1.1 <-- Generate token if doesn't exist, else reuse existing

start() ->
    ?LOG_INFO("Test", []),
    Dispatch = cowboy_router:compile([

        {'_', [
            {"/api/[...]", management_rest, []},
            {"/submit/[...]", raincloud_handler, []},
            {"/app/[...]", raincloud_handler, []},
            {"/", cowboy_static, {priv_file, raincloud, "web/index.html"}},
            {"/[...]", cowboy_static, {priv_dir, raincloud, "web/"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(raincloud_handler,
        [{port, 80}],
        #{env => #{dispatch => Dispatch}}
    ).


init(Req, Opts) ->
	Method = cowboy_req:method(Req),
	Path = cowboy_req:path(Req),
    {Ip, _Port} = cowboy_req:peer(Req),

    ?LOG_INFO("~p ~p from ~p", [Method, Path, Ip]),
    case handle_req(Method, Path, Req) of
        upgrade ->
            {cowboy_websocket, Req, #{ idle_timeout => 120000}};
        {Code, Req2} ->
            Reply = cowboy_req:reply(Code, Req2),
	        {ok, Reply, Opts};
        _ ->
            %TODO: forward to real service on ws.pusherapp.com
            ?LOG_WARNING("Received request for other service, ignoring...", []),
            Reply = cowboy_req:reply(501, Req),
	        {ok, Reply, Opts}
    end.


handle_req(<<"GET">>, <<"/app/", _/binary>>, Req) ->
    Origin = cowboy_req:header(<<"origin">>, Req, undefined),
    case Origin of
        <<"MelnorSprinklerSystem">> ->
	        ?LOG_INFO("Received request for melnor app, will upgrade to WS", []),
             upgrade;
        _ ->
            forward
    end;

handle_req(<<"GET">>, <<"/submit/">>, Req) ->
    QsVals = cowboy_req:parse_qs(Req),
    {_, Hash} = lists:keyfind(<<"idhash">>, 1, QsVals),
    {_, Message} = lists:keyfind(<<"message">>, 1, QsVals),
    ?LOG_INFO("Got hash ~p with message ~p QsVals:~p", [Hash, Message, QsVals]),
    handle_message(Hash, Message, Req);

handle_req(Method, Path, _Req) ->
    ?LOG_WARNING("Received request for unknown method path (~p ~p)", [Method, Path]),
    forward.

handle_message(Hash, <<"ascii--hashkeyevnt--ack--null">>, Req) ->
    ?LOG_INFO("Received Hash Ack ~p", [Hash]),
    case raincloud_store:get_hash_deviceid_mapping(Hash) of
        [] ->
            ?LOG_INFO("Unknown hash ~p", [Hash]),
            {401, Req};
        [{_, ChannelId}] ->
            set_automatic_schedule(ChannelId,0),
            {200, Req}

    end;

handle_message(Hash, <<"ascii--timestampevnt--ack--null">>, Req) ->
    ?LOG_INFO("Received timestampevnt ack", []),
    case raincloud_store:get_hash_deviceid_mapping(Hash) of
        [] ->
            ?LOG_INFO("Unknown hash ~p", [Hash]),
            {401, Req};
        [{_, ChannelId}] ->
            get_hardware_revision(ChannelId),
            {200, Req}

    end;

handle_message(_Hash, <<"ascii--manualctrlevnt--ack--null">>, Req) ->
    ?LOG_INFO("Received manual ctrl event ack", []),
    {200, Req};

handle_message(Hash, <<"ascii--Day6scheduleevnt--ack--null">>, Req) ->
    ?LOG_INFO("Received scheduleevnt ack 6", []),
    case raincloud_store:get_hash_deviceid_mapping(Hash) of
        [] ->
            ?LOG_INFO("Unknown hash ~p", [Hash]),
            {401, Req};
        [{_, ChannelId}] ->
            sync_timestamp(ChannelId),
            {200, Req}

     end;

handle_message(Hash, <<"ascii--Day", DayNb:8, "scheduleevnt--ack--null">>, Req) ->
    DayNb2 = list_to_integer([DayNb]), % Convert Ascii code of '0' to number 0
    ?LOG_INFO("Received scheduleevnt ack ~p", [DayNb2]),
        case raincloud_store:get_hash_deviceid_mapping(Hash) of
        [] ->
            ?LOG_INFO("Unknown hash ~p", [Hash]),
            {401, Req};
        [{_, ChannelId}] ->
            set_automatic_schedule(ChannelId,DayNb2+1),
            {200, Req}

    end;

handle_message(Hash, <<"ascii--revisions--", RevisionName/binary>>, Req) ->
    case raincloud_store:get_hash_deviceid_mapping(Hash) of
        [] ->
            ?LOG_INFO("Unknown hash ~p", [Hash]),
            {401, Req};
        [{_, ChannelId}] ->
            ?LOG_INFO("Hardware revision is  ~p", [RevisionName]),
            raincloud_unit:send_command(ChannelId, set_hw_rev, RevisionName),
            {200, Req}
     end;

handle_message(_Hash, <<"ascii", _/binary>> = Message, Req) ->
    ?LOG_INFO("Received unknown event ~p", [Message]),
    {200, Req};

handle_message(<<"0000000000">>, Message, Req) ->
    NewHash =  base64:encode(crypto:strong_rand_bytes(10)), %TODO: Store this in state and match on ack
    #status{ serial=ChannelId } = UnitStatus = state_decoder:get_status(Message),
    raincloud_store:add_hash_deviceid_mapping(NewHash, ChannelId),
    raincloud_unit:send_command(ChannelId, update_status, UnitStatus),
    ?LOG_INFO("UnitStatus ~p", [UnitStatus]),
    WsMessage = jsx:encode([{<<"event">>, <<"hash_key">>}, {<<"data">>, NewHash}, {<<"channel">>, ChannelId}]), %TODO: move to commands
    case raincloud_unit:send_command(ChannelId, send_ws, WsMessage) of
        ok ->
            {200, Req};
        fail ->
            {401, Req}
    end;

handle_message(Hash, Message, Req) ->
    ?LOG_INFO("Got message ~p for hash ~p", [Message, Hash]),
    #status{ serial=Channel } = UnitStatus = state_decoder:get_status(Message),
    ?LOG_INFO("UnitStatus ~p", [UnitStatus]),
    case raincloud_unit:send_command(Channel, update_status, UnitStatus) of
        ok ->
            {200, Req};
        fail ->
            {401, Req}
    end.

websocket_init(State) ->
    ?LOG_INFO("[WS] Init ~p", [self()]),
    %When using pusher:connection_established, the client doesn't call the next HTTP method, so we just skip it as it will work very well without it.
    %Body = jsx:encode([{<<"event">>, <<"pusher:connection_established">>}, {<<"data">>, <<"{\"socket_id\":\"242216.674885\"}">>}]),
    %?LOG_INFO("Connection established: ~p", [Body]),
    %{reply, {text, Body}, State}.
    {ok, State}.

websocket_handle({text, Body}, State) ->
    ?LOG_INFO("[WS] <= ~p", [Body]),
    JsonBody = jsx:decode(Body, [return_maps]),
    %{\"data\": {\"channel\": \"XYZDeviceId\"}, \"event\": \"pusher:subscribe\"}">>}
    Data = maps:get(<<"data">>, JsonBody, undefined),
    ?LOG_INFO("Data: ~p", [Data]),
    Channel = maps:get(<<"channel">>, Data, undefined),
    Event = maps:get(<<"event">>, JsonBody, undefined),
    case Event of
        <<"pusher:subscribe">> ->
            Reply = [{<<"event">>, <<"pusher_internal:subscription_succeeded">>}, {<<"data">>, <<"{}">>}, {<<"channel">>, Channel}],
            ReplyJson = jsx:encode(Reply),
            ?LOG_INFO("[WS] => ~p", [ReplyJson]),

            UnitPid = case raincloud_store:get_unit(Channel) of
                [] ->
                    {ok, Pid} = raincloud_unit:start_link(Channel),
                    raincloud_store:add_unit(Channel, Pid),
                    Pid;
                 [{_, Pid}] ->
                     Pid
            end,
            raincloud_unit:send_command(UnitPid, set_websocket, self()),
            ?LOG_INFO("Registering ~p to ~p", [self(), Channel]),
            {reply, {text, ReplyJson}, State};
        _ ->
            ?LOG_ERROR("Unknown event ~p, ignoring... ", [Event]),
            {ok, State}
    end;


websocket_handle(Frame, State) ->
    ?LOG_INFO("[WS] Handle ~p", [Frame]),
    {ok, State}.

websocket_info(ping, State) ->
    ?LOG_INFO("[WS] ping", [self()]),
   {reply, ping, State};

websocket_info(Message, State) ->
    ?LOG_INFO("[WS] Message ~p", [Message]),
    {reply, {text, Message}, State}.

terminate(Reason, #{host:=<<"ws.pusherapp.com">>}, _State) ->
    ?LOG_INFO("[WS] Terminate ~p with reason ~p", [self(), Reason]),
    ok;

terminate(Reason,Req, State) ->
    ok.
%TODO: Implement sync timestamp command at interval
%TODO: Implement schedule

get_hardware_revision(Channel) ->
    ?LOG_INFO("Sending hardware revision request", []),
    WsMessage = jsx:encode([{<<"event">>, <<"rev_request">>}, {<<"data">>, <<"">>}, {<<"channel">>, Channel}]), %TODO: move to commands
    raincloud_unit:send_command(Channel, send_ws, WsMessage).

set_automatic_schedule(Channel, Day) when (Day >= 0) and (Day =< 6) ->
    ?LOG_INFO("Sending automatic schedule for day ~p", [Day]),
    % Start setting automatic schedule
    EmptySchedule = <<"Ld0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=">>,
    WsMessage = jsx:encode([{<<"event">>, list_to_binary([<<"sched_day">>, integer_to_binary(Day)])}, {<<"data">>, EmptySchedule}, {<<"channel">>, Channel}]), %TODO: move to commands
    raincloud_unit:send_command(Channel, send_ws, WsMessage);

set_automatic_schedule(_,_) ->
    ok.

sync_timestamp(Channel) ->
    {Date,{Hours, Minutes, _Seconds}} = calendar:local_time(),
    ErlDayOfWeek = calendar:day_of_the_week(Date),
    DayOfWeek = erl_day_of_week_to_melnor(ErlDayOfWeek),
    CommandData = command:encode(timestamp, {Minutes, Hours, DayOfWeek}),
    Message = jsx:encode([{<<"event">>, <<"timestamp">>}, {<<"data">>, CommandData}, {<<"channel">>, Channel}]), %TODO: move to commands
    raincloud_unit:send_command(Channel, send_ws, Message).

% Erlang start on Monday = 1 while Melnor start with Sunday = 0
erl_day_of_week_to_melnor(7) ->
    0;

erl_day_of_week_to_melnor(Num) ->
    Num.
-module(management_rest).
-include_lib("kernel/include/logger.hrl").
-include("type.hrl").
-export([init/2]).


init(Req, Opts) ->
	Method = cowboy_req:method(Req),
	Path = cowboy_req:path(Req),
    {Ip, _Port} = cowboy_req:peer(Req),

    ?LOG_INFO("~p ~p from ~p", [Method, Path, Ip]),
    case handle_req(Method, Path, Req) of
        {Code, Req2} ->
            Reply = cowboy_req:reply(Code, Req2),
	        {ok, Reply, Opts};
        {Code, Headers, Message, Req2} ->
            Reply = cowboy_req:reply(Code, Headers, Message, Req2),
	        {ok, Reply, Opts};
        _ ->
            ?LOG_WARNING("Received request for other service, ignoring...", []),
            Reply = cowboy_req:reply(503, Req),
	        {ok, Reply, Opts}
    end.

handle_req(<<"GET">>, <<"/api/control">>, Req) ->
    QsVals = cowboy_req:parse_qs(Req),
    {_, Valve} = lists:keyfind(<<"valve">>, 1, QsVals),
    {_, DurationMin} = lists:keyfind(<<"duration">>, 1, QsVals),
    DeviceId = case lists:keyfind(<<"deviceid">>, 1, QsVals) of
        {_, Dev} ->
            Dev;
        _ ->
            raincloud_config:default_deviceid()
    end,
    case raincloud_unit:send_command(DeviceId, toggle_valve, {binary_to_integer(Valve), binary_to_integer(DurationMin)}) of
        fail ->
            {500, #{<<"content-type">> => <<"application/json">>}, jsx:encode(#{<<"error">> => <<"Unable to toggle valve">>}),Req};
        ok ->
            {200, Req}
    end;

handle_req(<<"GET">>, <<"/api/status">>, Req) ->
    QsVals = cowboy_req:parse_qs(Req),
    DeviceId = case lists:keyfind(<<"deviceid">>, 1, QsVals) of
        {_, Dev} ->
            Dev;
        _ ->
            raincloud_config:default_deviceid()
    end,
    case raincloud_unit:get_state(DeviceId) of
        fail ->
            {200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(#{<<"unit_status">>=> offline}),Req};
        #unit_state{status = #status{comm_link = CommLink, time = {Hours, Minutes}, day = Day, battery = Battery, valve1= V1, valve2 = V2, valve3 = V3, valve4 = V4},
            valve1_endtime = V1Endtime, valve2_endtime = V2Endtime, valve3_endtime = V3Endtime, valve4_endtime = V4Endtime} ->


            Body = jsx:encode([
                {<<"host_time">>, erlang:system_time(millisecond)},
                {<<"unit_status">>, online},
                {<<"valve_status">>, CommLink},
                {<<"valve_hours">>, Hours},
                {<<"valve_minutes">>, Minutes},
                {<<"valve_day">>, Day},
                {<<"valve_battery">>, Battery},
                {<<"valve1_status">>, V1},
                {<<"valve2_status">>, V2},
                {<<"valve3_status">>, V3},
                {<<"valve4_status">>, V4},
                {<<"valve1_endtime">>, V1Endtime},
                {<<"valve2_endtime">>, V2Endtime},
                {<<"valve3_endtime">>, V3Endtime},
                {<<"valve4_endtime">>, V4Endtime}
            ]),
            {200, #{<<"content-type">> => <<"application/json">>}, Body,Req}
    end;

handle_req(_, _, Req) ->
    {404, Req}.
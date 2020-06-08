-module(raincloud_unit).
-behaviour(gen_server).
-include("type.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send_command/3, get_state/1]).



start_link(Name) ->
   gen_server:start_link(?MODULE, [Name], []).

init(Name) ->
   ?LOG_INFO("Init: ~p", [Name]),
   %TODO: Set timer to periodically update timestamp of unit
   {ok, #unit_state{
      status = #status{serial = Name}
   }}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call({set_websocket, WebSocket}, _From, #unit_state{ws_keepalive_timer=CurrentTimer} = State) ->
   stop_timer(CurrentTimer),
   {ok, TRef} = timer:send_interval(30000, ping), % Must be less than cowboy idle timeout (Melnor implementation was at 200 second, we set the ping at 60 sec with idle_timeout at 120 sec)
   {reply, ok, State#unit_state{ws = WebSocket, last_communication = erlang:system_time(millisecond), ws_keepalive_timer = TRef}};

handle_call({update_status, Status}, _From, #unit_state{status=OldStatus} = State) ->
   V1EndTime = get_end_time(OldStatus#status.valve1, Status#status.valve1,  State#unit_state.valve1_endtime),
   V2EndTime = get_end_time(OldStatus#status.valve2, Status#status.valve2,  State#unit_state.valve2_endtime),
   V3EndTime = get_end_time(OldStatus#status.valve3, Status#status.valve3,  State#unit_state.valve3_endtime),
   V4EndTime = get_end_time(OldStatus#status.valve4, Status#status.valve4,  State#unit_state.valve4_endtime),
   {reply, ok, State#unit_state{status = Status, last_communication = erlang:system_time(millisecond), valve1_endtime = V1EndTime, valve2_endtime = V2EndTime, valve3_endtime = V3EndTime, valve4_endtime = V4EndTime}};

handle_call({set_hw_rev, HwRev}, _From, State) ->
    {reply, ok, State#unit_state{hardware_revision = HwRev, last_communication = erlang:system_time(millisecond)}};

handle_call({get_state}, _From, #unit_state{} = State) ->
    {reply, State, State};

handle_call({toggle_valve, {ValveIdx, DurationMin}}, _From, #unit_state{status = #status{serial = Channel, valve_id= ValveId}, ws = Ws} = State) ->
   case get_toggle_valve_msg(Channel, ValveId, ValveIdx, DurationMin) of
      error ->
         {reply, error, State};
      Message ->
         Ws ! Message,
         % TODO: Race condition here if we receive a status before the toggle change as this will be erased by the get_end_time method
         EndTime = erlang:system_time(millisecond) + DurationMin*60000,
         State2 = case ValveIdx of
            1 ->
               State#unit_state{valve1_endtime = EndTime};
            2 ->
               State#unit_state{valve2_endtime = EndTime};
            3 ->
               State#unit_state{valve3_endtime = EndTime};
            4 ->
               State#unit_state{valve4_endtime = EndTime}
         end,
         {reply, ok, State2}
   end;

handle_call({send_ws, Message}, _From, #unit_state{ws = undefined} = State) ->
   ?LOG_WARNING("No ws, won't send message ~p", [Message]),
    {reply, fail, State};

handle_call({send_ws, Message}, _From, #unit_state{ws = Ws} = State) ->
   Ws ! Message,
   {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(ping, #unit_state{ws = Ws, ws_keepalive_timer = RefTimer} = State) ->
   State2 = case is_process_alive(Ws) of
      true ->
         ?LOG_INFO("Sending ping for ~p", [Ws]),
         Ws ! ping,
         State;
      false ->
         ?LOG_INFO("Websocket is dead for ~p, stopping ping", [Ws]),
         stop_timer(RefTimer),
         State#unit_state{ws=undefined, ws_keepalive_timer = undefined}
   end,
   {noreply, State2};

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

stop_timer(undefined) ->
   ok;

stop_timer(TRef) ->
   timer:cancel(TRef).

% Toggle to off
get_end_time(_, off, _) ->
   undefined;

get_end_time(State, State, EndTime) ->
   EndTime;

% Toggle to manual
get_end_time(_, manual, _) -> % 1 hour on manual toggle
   erlang:system_time(millisecond) + 3600000;


% Toggle to automatic
get_end_time(_, _, EndTime) ->
   EndTime.
-spec send_command(pid()|binary(), update_status, #status{}) -> fail|ok;
                  (pid()|binary(), send_ws, binary()) -> fail|ok;
                  (pid()|binary(), set_websocket, pid()) -> fail|ok;
                  (pid()|binary(), toggle_valve, {non_neg_integer(), non_neg_integer()}) -> fail|ok;
                  (pid()|binary(), set_hw_rev, binary()) -> fail|ok.

send_command(UnitPid, Command, Message) when is_pid(UnitPid) ->
   gen_server:call(UnitPid, {Command, Message});

send_command(ChannelId, Command, Message) when is_binary(ChannelId) ->
     case raincloud_store:get_unit(ChannelId) of
      [] ->
         ?LOG_ERROR("Unknown ChannelId ~p for command ~p, fail", [ChannelId, Command]),
         fail;
      [{_, Pid}] ->
         send_command(Pid, Command, Message)
   end.

-spec get_state(pid()|binary()) ->
   fail|#unit_state{}.

get_state(UnitPid) when is_pid(UnitPid) ->
   gen_server:call(UnitPid, {get_state});

get_state(ChannelId) when is_binary(ChannelId) ->
   case raincloud_store:get_unit(ChannelId) of
      [] ->
         ?LOG_ERROR("Requested state for unknown unit, ok"),
         fail;
      [{_, Pid}] ->
         get_state(Pid)
   end.

get_toggle_valve_msg(undefined, _, _, _) ->
   ?LOG_ERROR("Channel undefined", []),
   error;

get_toggle_valve_msg(_, undefined, _, _) ->
   ?LOG_ERROR("ValveId undefined", []),
   error;

get_toggle_valve_msg(Channel, ValveId, ValveIdx, DurationMin) ->
   ?LOG_INFO("C:~p V:~p Idx: ~p Duration: ~p", [Channel, ValveId, ValveIdx, DurationMin]),
   get_toggle_valve_msg(Channel, ValveId, ValveIdx, DurationMin, [off, off, off, off]).

get_toggle_valve_msg(Channel, ValveId, ValveIdx, 0, CurrentValveState) ->
   %TODO: De-duplicate
   NewValveState = setnth(ValveIdx, CurrentValveState, off),
   CommandData = command:encode(manual_sched,  erlang:append_element(list_to_tuple(NewValveState), ValveId)),
   ?LOG_INFO("C:~p V:~p Idx: ~p Closing valve", [Channel, ValveId, ValveIdx]),
   jsx:encode([{<<"event">>, <<"manual_sched">>}, {<<"data">>, CommandData}, {<<"channel">>, Channel}]);

get_toggle_valve_msg(Channel, ValveId, ValveIdx, DurationMin, CurrentValveState) when (DurationMin < 1440) and (DurationMin > 0) ->
    {_Date, {Hour, Minute, _Seconds}} = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(calendar:local_time()) + DurationMin*60),
    NewTuple = patch_time({Hour, Minute}),
    NewValveState = setnth(ValveIdx, CurrentValveState, NewTuple),
    CommandData = command:encode(manual_sched,  erlang:append_element(list_to_tuple(NewValveState), ValveId)),
   ?LOG_INFO("C:~p V:~p Idx: ~p Set valve for ~p min", [Channel, ValveId, ValveIdx, DurationMin]),
    jsx:encode([{<<"event">>, <<"manual_sched">>}, {<<"data">>, CommandData}, {<<"channel">>, Channel}]);

get_toggle_valve_msg(_Channel, _ValveId, ValveId, DurationMin, _) ->
    ?LOG_WARNING("Invalid input ~p for valve ~p", [DurationMin, ValveId]),
    error.

setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

patch_time({0,0}) ->
    {0, 1}; % {0,0} is off

patch_time(T) ->
    T.

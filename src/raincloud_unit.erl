-module(raincloud_unit).
-behaviour(gen_server).
-include("type.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send_command/3, get_status/1]).

-record(state, {
   status :: #status{},
   last_communication :: non_neg_integer(), 
   hardware_revision :: binary(),
 %  hash :: binary(),
   ws
}).

%start(Name) ->
   %_sup:start_child(Name).

%stop(Name) ->
%   gen_server:call(Name, stop).

start_link(Name) ->
   gen_server:start_link(?MODULE, [Name], []).

init(Name) ->
   ?LOG_INFO("Init: ~p", [Name]),
   %TODO: Set timer to periodically update timestamp of unit
   {ok, #state{
      status = #status{serial = Name}
   }}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call({set_websocket, WebSocket}, _From, State) ->
    {reply, ok, State#state{ws = WebSocket, last_communication = erlang:system_time(millisecond)}};

handle_call({update_status, Status}, _From, State) ->
    {reply, ok, State#state{status = Status, last_communication = erlang:system_time(millisecond)}};

handle_call({set_hw_rev, HwRev}, _From, State) ->
    {reply, ok, State#state{hardware_revision = HwRev, last_communication = erlang:system_time(millisecond)}};

handle_call({get_status}, _From, #state{status = Status} = State) ->
    {reply, Status, State};

handle_call({toggle_valve, {ValveIdx, DurationMin}}, _From, #state{status = #status{serial = Channel, valve_id= ValveId}, ws = Ws} = State) ->
   case get_toggle_valve_msg(Channel, ValveId, ValveIdx, DurationMin) of
      error ->
         {reply, error, State};
      Message ->
          Ws ! Message,
         {reply, ok, State}
   end;

handle_call({send_ws, Message}, _From, #state{ws = undefined} = State) ->
   ?LOG_WARNING("No ws, won't send message ~p", [Message]),
    {reply, fail, State};

handle_call({send_ws, Message}, _From, #state{ws = Ws} = State) ->
   Ws ! Message,
   {reply, ok, State};


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

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

-spec get_status(pid()|binary()) ->
   fail|#status{}.

get_status(UnitPid) when is_pid(UnitPid) ->
   gen_server:call(UnitPid, {get_status});

get_status(ChannelId) when is_binary(ChannelId) ->
   case raincloud_store:get_unit(ChannelId) of
      [] ->
         ?LOG_ERROR("Requested state for unknown unit, ok"),
         fail;
      [{_, Pid}] ->
         get_status(Pid)
   end.


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

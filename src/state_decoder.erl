-module(state_decoder).
-export([decode/1, get_status/1]).
-export([valve_state/1]).
-include("type.hrl").



get_status(Msg) ->
    DecodedMsg = decode(Msg),
    <<RfSerial:48/little, Day:8, 0:8, MinutesSinceMidnight:16/little, ValveSerial:16/little, ValveState:8, Battery:8, 0:8, ValveCommStatus:8, _/binary>> = DecodedMsg,
    {Valve1, Valve2, Valve3, Valve4} = valve_state(ValveState),
    #status{serial = to_binary(RfSerial), 
                         valve_id = to_binary(ValveSerial),
                         day = day_to_atom(Day),
                         time = minutes_to_time(MinutesSinceMidnight),
                         comm_link = comm_link_status(ValveCommStatus),
                         battery = battery(Battery),
                         valve1 = Valve1,
                         valve2 = Valve2,
                         valve3 = Valve3,
                         valve4 = Valve4
    }.

decode(Msg) ->
    % Decoding message for raincloud is like base64 except char + is a - like in base64url
    NewMsg = binary:replace(Msg, <<"-">>, <<"+">>),
    base64:decode(NewMsg).

valve_state(ValveState) ->
    ManualHalfByte =  ValveState rem 16,
    AutomaticHalfByte = (ValveState-ManualHalfByte) bsr 4,
    <<_:4, M4:1, M3:1, M2:1, M1:1>> = <<ManualHalfByte/integer>>,
    <<_:4, A4:1, A3:1, A2:1, A1:1>> = <<AutomaticHalfByte/integer>>,
    {valve_to_atom(A1, M1), valve_to_atom(A2, M2), valve_to_atom(A3, M3), valve_to_atom(A4, M4)}.

%Automatic, Manual
valve_to_atom(0,0) ->
    off;
valve_to_atom(1,_) ->
    automatic;
valve_to_atom(0,1) ->
    manual.    

% Determined empirically with the web interface
battery(Battery) ->
    round(100*(Battery - 187)/68). 

comm_link_status(0) ->
    online;

comm_link_status(1) ->
    temporary_disconnect;

comm_link_status(2) ->
    offline.

minutes_to_time(MinutesSinceMidnight) ->
    Minutes = MinutesSinceMidnight rem 60,
    Hours = round((MinutesSinceMidnight - Minutes)/60),
    {Hours, Minutes}.

day_to_atom(0) ->
    sunday;
day_to_atom(1) ->
    monday;
day_to_atom(2) ->
    tuesday;
day_to_atom(3) ->
    wednesday;
day_to_atom(4) ->
    thursday;
day_to_atom(5) ->
    friday;
day_to_atom(6) ->
    sathurday.

to_binary(Int) ->
    list_to_binary(httpd_util:integer_to_hexlist(Int)).

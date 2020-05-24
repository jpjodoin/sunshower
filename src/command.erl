-module(command).
-export([encode/2]).
%manual_sched|timestamp|sched_day0-sched_day6


encode(timestamp, {Minutes, Hours, Day}) ->
   MinutesSinceStartOfDay = time_to_min({Hours, Minutes}),
   base64:encode(<<MinutesSinceStartOfDay:16/little, Day>>);

encode(manual_sched, {ValveStop1, ValveStop2, ValveStop3, ValveStop4, ValveId}) ->
    Min1 = time_to_min(ValveStop1),    
    Min2 = time_to_min(ValveStop2),    
    Min3 = time_to_min(ValveStop3),    
    Min4 = time_to_min(ValveStop4),    
    ValveIdInt = binary_to_integer(ValveId, 16),
    Bin = <<ValveIdInt:16/little, 
        Min1:16/little,
        Min2:16/little,
        Min3:16/little,
        Min4:16/little,
        0:64/little>>,
    base64:encode(Bin).

time_to_min(off) ->
    0;

time_to_min({H,M}) ->
    H*60+M.









-module(command_test).
-include_lib("eunit/include/eunit.hrl").

encode_manual_sched_test() ->
    EncodedMsg = command:encode(manual_sched, {{23,59}, off, off, off, <<"DD2D">>}),
    Expected = <<"Ld2fBQAAAAAAAAAAAAAAAAAA">>,
    ?assertEqual( Expected, EncodedMsg).

% encode_timestamp_test() ->


%     EncodedData = command:encode(timestamp, 463),
%     Expected = <<"zwED">>,
%     ?assertEqual(Expected, EncodedData).

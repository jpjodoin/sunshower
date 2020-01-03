
-module(command_test).


-include_lib("eunit/include/eunit.hrl").


encode_manual_sched_test() ->
    EncodedMsg = command:encode(manual_sched, {<<"DD2D">>, {23,59}, off, off, off}),
    Expected = <<"Ld2fBQAAAAAAAAAAAAAAAAAA">>,
    ?assertEqual( Expected, EncodedMsg).


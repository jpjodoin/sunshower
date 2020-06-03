
-module(state_decoder_test).


-include_lib("eunit/include/eunit.hrl").


state_decoding_test() ->
    Msg = <<"1xOuOYDYAwDQAS3dAP8AAgAAAAAAAg==">>,
    %message_decoder:log(<<"1xOuOYDYBABoBS3dEf0AAAAAAAAAAg==">>),
    DecodedMsg = state_decoder:decode(Msg),
    Expected = <<16#d7,16#13,16#ae,16#39,16#80,16#d8,16#03,16#00,16#d0,16#01,16#2d,16#dd,16#00,16#ff,16#00,16#02,16#00,16#00,16#00,16#00,16#00,16#02>>,
    ?assertEqual( Expected, DecodedMsg),

    Msg2 = <<"1xOuOYDYBQAZBS3dD/8AAAAAAAAAAg==">>,
    DecodedMsg2 = state_decoder:decode(Msg2),
    Expected2 = <<16#d7,16#13,16#ae,16#39,16#80,16#d8,16#05,16#00,16#19,16#05,16#2d,16#dd,16#0f,16#ff,16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#02>>,
    ?assertEqual( Expected2, DecodedMsg2),

    Msg3 = <<"1xOuOYDYAwA-Ai3dAP8AAAAAAAAAAg==">>,
    DecodedMsg3 = state_decoder:decode(Msg3),
    Expected3 = <<16#d7,16#13,16#ae,16#39,16#80,16#d8,16#03,16#00,16#3e,16#02,16#2d,16#dd,16#00,16#ff,16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#02>>,
    ?assertEqual( Expected3, DecodedMsg3).

valve_test() ->
    Res = state_decoder:valve_state(16#0),
    Expect = {off, off, off, off},
    ?assertEqual( Expect, Res),
    Res1 = state_decoder:valve_state(16#11),
    Expect1 = {automatic, off, off, off},
    ?assertEqual( Expect1, Res1),
    Res2 = state_decoder:valve_state(16#33),
    Expect2 = {automatic, automatic, off, off},
    ?assertEqual( Expect2, Res2),
    Res3 = state_decoder:valve_state(16#18),
    Expect3 = {automatic, off, off, manual},
    ?assertEqual( Expect3, Res3),
    Res4 = state_decoder:valve_state(16#67),
    Expect4 = {manual, automatic, automatic, off},
    ?assertEqual( Expect4, Res4).

get_state_test() ->
    %TODO: add more example
    Msg = <<"1xOuOYDYAwDQAS3dAP8AAgAAAAAAAg==">>,
    State = state_decoder:get_status(Msg),
    ?assertEqual({status,<<"d88039ae13d7">>,<<"DD2D">>,wednesday,{7,44},offline,100,off,off,off,off}, State).

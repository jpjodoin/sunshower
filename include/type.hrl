-record(status, {
    serial :: binary(), % Binary string with serial number as displayed on the RfLink
    valve_id :: binary(), %Binary string with the valve id as displayed on the Valve
    day :: sunday|monday|tuesday|wednesday|thursday|friday|sathurday,
    time :: {non_neg_integer(), non_neg_integer()},
    comm_link :: online|temporary_disconnect|offline,
    battery :: non_neg_integer(), % Current battery status between 0 and 100%, will not report exactly the same value as the web interface since the web interface one is limited to 99% and skip all multiple of 3 for some odd reason
    valve1 :: manual|automatic|off,
    valve2 :: manual|automatic|off,
    valve3 :: manual|automatic|off,
    valve4 :: manual|automatic|off
}
).
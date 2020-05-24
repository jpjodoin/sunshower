-module(raincloud_store).
-export([start/0, add_hash_deviceid_mapping/2, add_unit/2, get_hash_deviceid_mapping/1, get_unit/1]).
-include_lib("kernel/include/logger.hrl").

start() ->
    Res = ets:new(raincloudunit, [set, named_table, public]),
    Res2 = ets:new(hash_deviceid_map, [set, named_table, public]),
    ?LOG_INFO("Table creation Res:~p Res:~p", [Res, Res2]).

add_hash_deviceid_mapping(Hash, DeviceId) ->
    ets:insert(hash_deviceid_map, {Hash, DeviceId}).

get_hash_deviceid_mapping(Hash) ->
    ets:lookup(hash_deviceid_map, Hash).

add_unit(DeviceId, Pid) ->
    ets:insert(raincloudunit, {DeviceId, Pid}).

get_unit(DeviceId) ->
    ets:lookup(raincloudunit, DeviceId).
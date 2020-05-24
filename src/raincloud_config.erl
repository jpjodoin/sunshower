-module(raincloud_config).
-export([default_deviceid/0]).

default_deviceid() ->
    string:lowercase(application:get_env(raincloud, default_deviceid, undefined)).
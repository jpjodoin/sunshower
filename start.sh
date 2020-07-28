#!/bin/bash
set -e
# This is only required to do variable substition in index.html
export VALVE1_LABEL="Valve 1"
export VALVE2_LABEL="Valve 2"
export VALVE3_LABEL="Valve 3"
export VALVE4_LABEL="Valve 4"
if ! command -v envsubst  &> /dev/null
then
    echo "envsubst could not be found, install the gettext package"
    exit
fi
envsubst < priv/web/index.templ.html > priv/web/index.html

# You will need to have rebar3 and erlang OTP 22 to run this (it can probably work on lower version, I have not tried)
# sudo is necessary here to run on port 80
sudo rebar3 shell
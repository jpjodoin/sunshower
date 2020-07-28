# TODO: Below path is a bit hackish, we will need to do this differently if we want to have a read-only container
envsubst < /opt/rel/lib/raincloud-0.0.0/priv/web/index.templ.html > /opt/rel/lib/raincloud-0.0.0/priv/web/index.html
/opt/rel/bin/raincloud foreground

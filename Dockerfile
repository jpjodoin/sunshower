FROM erlang:22.3.4.1-alpine as base
FROM base as builder
RUN apk add --no-cache --update git

WORKDIR /usr/src/app/

# build and cache dependencies
COPY rebar.config rebar.lock /usr/src/app/
RUN rebar3 compile
# Copy the source and build release
COPY config config
COPY src src
COPY include include
COPY priv priv
RUN rebar3 as prod tar

RUN mkdir -p /opt/rel
RUN tar -zxvf /usr/src/app/_build/prod/rel/*/*.tar.gz -C /opt/rel

FROM base
# This is required for envsubst
RUN apk add --no-cache --update gettext
ENV VALVE1_LABEL "Valve 1"
ENV VALVE2_LABEL "Valve 2"
ENV VALVE3_LABEL "Valve 3"
ENV VALVE4_LABEL "Valve 4"

COPY --from=builder /opt/rel/ /opt/rel/
COPY scripts/docker-entrypoint.sh /opt/rel/bin/docker-entrypoint.sh
WORKDIR /opt/rel/
EXPOSE 80
ENTRYPOINT /opt/rel/bin/docker-entrypoint.sh
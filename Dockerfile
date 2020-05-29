FROM erlang:22.3.4.1-alpine as base
FROM base as builder
RUN apk add --no-cache --update  git

WORKDIR /usr/src/app/

# build and cache dependencies
COPY rebar.config rebar.lock /usr/src/app/
RUN rebar3 compile
# Copy the source and build release
COPY . /usr/src/app/
RUN rebar3 as prod tar

RUN mkdir -p /opt/rel
RUN tar -zxvf /usr/src/app/_build/prod/rel/*/*.tar.gz -C /opt/rel

FROM base

COPY --from=builder /opt/rel/ /opt/rel/
WORKDIR /opt/rel/
EXPOSE 80
ENTRYPOINT /opt/rel/bin/raincloud foreground
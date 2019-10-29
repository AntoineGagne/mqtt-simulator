FROM erlang:21-alpine as erlang-build
RUN apk add --no-cache git=2.20.1-r0
WORKDIR /mqtt_simulator
COPY rebar.config rebar.lock /mqtt_simulator/
RUN rebar3 compile
COPY . /mqtt_simulator
RUN rebar3 as prod tar -n mqtt_simulator -o /opt

FROM alpine:3.9
RUN apk add --no-cache \
    musl=1.1.20-r4 \
    libcrypto1.1=1.1.1b-r1 \
    ncurses-libs=6.1_p20190105-r0 \
    unixodbc=2.3.7-r0 \
    libcurl=7.64.0-r3 \
    libuuid=2.33-r0
# hadolint ignore=DL3010
COPY --from=erlang-build /opt/mqtt_simulator/mqtt_simulator-*.tar.gz /opt
RUN set -e \
    && mkdir -p /opt/mqtt_simulator \
    && tar -xzf /opt/mqtt_simulator-*.tar.gz -C /opt/mqtt_simulator \
    && rm /opt/mqtt_simulator-*.tar.gz
WORKDIR /opt/mqtt_simulator

RUN set -e \
    && addgroup mqtt_simulator \
    && adduser mqtt_simulator -h /home/mqtt_simulator -S -G mqtt_simulator \
    && chown -R mqtt_simulator:mqtt_simulator .
RUN chown -R mqtt_simulator:mqtt_simulator .
USER mqtt_simulator:mqtt_simulator
ENTRYPOINT ["bin/mqtt_simulator"]
CMD ["foreground"]

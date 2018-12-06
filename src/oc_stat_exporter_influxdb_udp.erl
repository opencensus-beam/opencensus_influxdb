-module(oc_stat_exporter_influxdb_udp).

-behaviour(oc_stat_exporter).

-export([export/2]).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 8089).

export(ViewData, Options) ->
    Host = proplists:get_value(host, Options, ?DEFAULT_HOST),
    Port = proplists:get_value(port, Options, ?DEFAULT_PORT),
    {ok, Socket} = gen_udp:open(0, [{active, false}]),
    Data = opencensus_influxdb:build_metrics(ViewData),
    ok = gen_udp:send(Socket, Host, Port, [Data, $\n]),
    ok = gen_udp:close(Socket),
    ok.

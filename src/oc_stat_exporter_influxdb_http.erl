-module(oc_stat_exporter_influxdb_http).

-behaviour(oc_stat_exporter).

-export([export/2]).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_URL, "http://localhost:8086").
-define(DEFAULT_DB, "opencensus").

export(ViewData, Options) ->
    URL = proplists:get_value(url, Options, ?DEFAULT_URL),
    DB = proplists:get_value(db, Options, ?DEFAULT_DB),
    Address = io_lib:format("~s/write?db=~s", [URL, DB]),
    Data = erlang:list_to_binary(opencensus_influxdb:build_metrics(ViewData)),
    case httpc:request(
           post,
           {Address, [], "", <<Data/binary, "\n">>},
           [],
           []
          ) of
        {ok, {{_, Code, _}, _, _}} when Code >= 200, Code =< 299 ->
            ok;
        {ok, {{_, Code, _}, _, Message}} ->
            ?LOG_ERROR("InfluxDB: Unable to send metrics, InfluxDB reported an error: ~p: ~p",
                       [Code, Message]);
        {error, Reason} ->
            ?LOG_ERROR("InfluxDB: Unable to send metrics, client error: ~p",
                       [Reason])
    end.

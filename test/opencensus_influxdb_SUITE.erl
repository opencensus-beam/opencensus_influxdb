-module(opencensus_influxdb_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     metrics_has_normalised_names
    ].

metrics_has_normalised_names(_Config) ->
    Metric = #{
      name => "com.influxdata/example/metric",
      ctags => #{},
      tags => [],
      data => #{
        type => counter,
        rows => [
                 #{tags => [],
                   value => 10}
                ]
       }
     },
    Output = build_output(Metric),
    ct:pal("~s", [Output]),
    ?assertEqual(nomatch, binary:match(Output, <<"/">>)),
    ?assertNotMatch(nomatch, binary:match(Output, <<"value=10">>)),
    ?assertMatch(<<"com.influxdata_example_metric", Rest/binary>>, Output),
    ok.

build_output(Metric) ->
    IoList = opencensus_influxdb:build_metrics(Metric),

    iolist_to_binary(IoList).

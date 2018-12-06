-module(opencensus_influxdb).

-export([build_metrics/1]).

build_metrics(Data) when is_list(Data) ->
    lists:join($\n, [build_metrics(Entry) || Entry <- Data]);
build_metrics(#{name := Name,
               ctags := CTags,
               tags := Tags,
               data := #{type := Type,
                         rows := Rows}}) ->
    TS = erlang:integer_to_binary(os:system_time(seconds)),
    Key = normalise(Name),
    Metrics = [build_row(Key, Type, TS, {CTags, Tags}, Row) || Row <- Rows],
    lists:join($\n, Metrics).

build_row(Key, Type, TS, {CTags, Tags}, #{tags := TagsV, value := Value}) ->
    Values = lists:join($,, values(Type, Value)),
    [Key, $,, build_tags(Tags, TagsV, CTags), $\ , Values, $\ , TS].

build_tags(Tags, TagsV, CTags) ->
  build_tags(maps:merge(CTags, maps:from_list(lists:zip(Tags, TagsV)))).

build_tags(Map) when is_map(Map) -> build_tags(maps:to_list(Map));
build_tags([]) -> [];
build_tags(Tags) ->
    List = [[normalise(Key), $=, normalise(Value)] || {Key, Value} <- Tags],
    lists:join($,, List).

normalise(Atom) when is_atom(Atom) ->
    normalise(erlang:atom_to_binary(Atom, utf8), <<>>);
normalise(String) when is_list(String) ->
    normalise(erlang:list_to_binary(String), <<>>);
normalise(String) when is_binary(String) -> normalise(String, <<>>).

normalise(<<>>, Result) -> Result;
normalise(<<",", Rest/binary>>, Result) -> normalise(Rest, <<Result/binary, "\\,">>);
normalise(<<" ", Rest/binary>>, Result) -> normalise(Rest, <<Result/binary, "\\ ">>);
normalise(<<"=", Rest/binary>>, Result) -> normalise(Rest, <<Result/binary, "\\=">>);
normalise(<<"\"", Rest/binary>>, Result) -> normalise(Rest, <<Result/binary, "\\\"">>);
normalise(<<C, Rest/binary>>, Result) -> normalise(Rest, <<Result/binary, C>>).

values(sum, #{count := Count,
              mean := Mean,
              sum := Sum}) ->
    [
     ["count=", format_num(Count)],
     ["mean=", format_num(Mean)],
     ["sum=", format_num(Sum)]
    ];
values(distribution, #{buckets := Buckets} = Data) ->
    BucketsData = [[format_num(Bound), $=, format_num(Value)]
                   || {Bound, Value} <- Buckets],
    values(sum, Data) ++ BucketsData;
values(_Type, Data) -> [["value=", format_num(Data)]].

format_num(infinity) -> <<"infinity">>;
format_num(Integer) when is_integer(Integer) ->
    erlang:integer_to_binary(Integer);
format_num(Float) when is_float(Float) ->
    erlang:float_to_binary(Float, [{decimals, 5}, compact]).

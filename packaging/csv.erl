% anonymous function
% in: <file name of source>
% out: <input for code_change function>
fun(SourceFN) when is_list(SourceFN) ->
    {ok, Source} = file:read_file(SourceFN),
    Map = lists:map(fun(Line) ->
                      case binary:split(Line, <<",">>, [global]) of
                          [Key | Value] when Value =/= [] ->
                              {Key, list_to_tuple(Value)};
                          _ -> undefined
                      end
              end, binary:split(Source, <<"\n">>, [global])),
    lists:filter(fun(undefined) -> false; (_) -> true end, Map)
end.

fun(Source) when is_binary(Source) ->
    lists:foldl(fun(<<>>, Tree) -> Tree;
                 (Binary, Tree) ->
                      {ok, Scanned, _} = erl_scan:string(binary_to_list(Binary)),
                      {ok, Parsed} = erl_parse:parse_exprs(Scanned),
                      {value, Tuple, _} = erl_eval:exprs(Parsed,[]),
                      case erl_eval:exprs(Parsed,[]) of
                          {value, Tuple, _} when is_tuple(Tuple) ->
                              [K | ValueList] = tuple_to_list(Tuple),
                              gb_trees:enter(K, list_to_tuple(ValueList), Tree);
                          _ -> Tree
                      end
              end, gb_trees:empty(), binary:split(Source, <<"\n">>, [global]))
end.

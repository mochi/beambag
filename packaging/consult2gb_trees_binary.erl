fun(SourceFN) when is_list(SourceFN) ->

        TransformList =
            fun(List, Transform) when is_list(List) ->
                    case lists:all(fun erlang:is_integer/1, List) of
                        true ->
                            unicode:characters_to_binary(List);
                        false ->
                            [Transform(V, Transform) || V <- List]
                    end;
               (Term, _Transform) ->
                    Term
            end,

        Transform =
            fun([_|_] = List, Transform) ->
                    TransformList(List, Transform);
               ({Key}, Transform) ->
                    {TransformList(Key, Transform), []};
               (Tuple, Transform) when is_tuple(Tuple) ->
                    list_to_tuple(TransformList(tuple_to_list(Tuple), Transform));
               (Term, _Transform) ->
                    Term
            end,

        {ok, Terms} = file:consult(SourceFN),
        Terms2 = Transform(Terms, Transform),
        gb_trees:from_orddict(lists:usort(Terms2))
end.

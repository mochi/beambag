fun(SourceFN) when is_list(SourceFN) ->
    {ok, Terms} = file:consult(SourceFN),
    gb_trees:from_orddict(lists:usort(Terms))
end.

fun(SourceFN) when is_list(SourceFN) ->
    {ok, Terms} = file:consult(SourceFN),
    Terms2 = case hd(Terms) of
                 {K} -> [{K1, []} || {K1} <- Terms];
                 _ -> Terms
             end,
    gb_trees:from_orddict(lists:usort(Terms2))
end.

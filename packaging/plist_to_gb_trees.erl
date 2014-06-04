% anonymous function
% in: <name of target module> <new data — output from parser function>
% out: <$$magic substitute>
fun(_Module, NewData) ->
    gb_trees:from_orddict(lists:usort(NewData))
end.

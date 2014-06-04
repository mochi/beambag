% anonymous function
% in: <name of target module> <new data — output from parser function>
% out: <$$magic substitute>
fun(Module, NewData) ->
    lists:foldl(fun({Key, Value}, Tree) -> gb_trees:enter(Key, Value, Tree) end, Module:get_current_tree(), NewData)
end.

%% '$$module' is replaced by the name of target module and {'$$magic'} is replaced by new data
-module('$$module').
-export([is_defined/1, get_value/1, get_value/2]).

is_defined(Key) ->
    gb_trees:is_defined(Key, tree()).

get_value(Key) ->
    get_value(Key, undefined).

get_value(Key, Default) ->
    case gb_trees:lookup(Key, tree()) of
        none -> Default;
        {value, Value} -> Value
    end.

tree() ->
    {'$$magic'}.

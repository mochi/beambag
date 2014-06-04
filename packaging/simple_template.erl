-module(simple_template).
-export([data/0]).

data() ->
    {'$$magic'}.

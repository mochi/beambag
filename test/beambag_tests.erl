-module(beambag_tests).

-include_lib("eunit/include/eunit.hrl").

-export([build/1, build_raw/1]).


build_raw(FileName) ->
    {ok, Data} = file:read_file(FileName),
    build([binary_to_term(Data)]).


build([MapData]) ->
    Map = proplists:get_value(<<"testmap">>, MapData, []),
    gb_trees:from_orddict(Map).


child_spec_test() ->
    ?assertMatch([{test_mapping_edit,
                   {beambag,start_link,
                    [test_mapping,"./../priv/test.etf","./../ebin/test.beam",
                     _]},
                   permanent,5000,worker,
                   [test_mapping_edit]},
                  [beambag]],
                  beambag:child_spec(test_mapping,
                                     beambag_deps:local_path(
                                       ["..", "priv", "test.etf"]),
                                     beambag_deps:local_path(
                                       ["..", "ebin", "test.beam"]),
                                     fun build_raw/1)),
    ok.

beambag_test_() ->
    {foreach,
     fun() ->
             {ok, Pid} = beambag:start_link(test_mapping,
                                            beambag_deps:local_path(
                                              ["..", "priv", "test.etf"]),
                                            beambag_deps:local_path(
                                              ["..", "ebin", "test.beam"]),
                                            {raw, fun ?MODULE:build_raw/1}),
             Pid
     end,
     fun(Pid) ->
             ok = gen_server:call(Pid, stop)
     end,
     [ fun last_updated/0,
       fun bad_req/0
     ]
    }.

last_updated() ->
    LastUpdated = beambag:last_updated(test_mapping),
    ?assertMatch({{_Year, _Month, _Day}, {_H, _M, _S}}, LastUpdated),
    ok.


bad_req() ->
    ?assertEqual({error, badreq}, gen_server:call(test_mapping_edit, bad_request)),
    ok.


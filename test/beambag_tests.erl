-module(beambag_tests).

-include_lib("eunit/include/eunit.hrl").

-include("beambag_edit_magic.hrl").

-export([build/1, build_raw/1]).


build_raw(FileName) ->
    {ok, Data} = file:read_file(FileName),
    build([binary_to_term(Data)]).


build([MapData]) ->
    Map = proplists:get_value(<<"testmap">>, MapData, []),
    gb_trees:from_orddict(Map).


child_spec_test() ->
    ?assertMatch([{beambag_mapping_example_edit,
                   {beambag,start_link,
                    [beambag_mapping_example,
                     "./../priv/test.txt",
                     "./../ebin/beambag_mapping_example.beam",
                     _]},
                   permanent,5000,worker,
                   [beambag_mapping_example_edit]},
                  [beambag]],
                  beambag:child_spec(beambag_mapping_example,
                                     "./../priv/test.txt",
                                     "./../ebin/beambag_mapping_example.beam",
                                     fun build/1)),
    ok.

beambag_test_() ->
    {foreach,
     fun() ->
             {ok, Pid} = beambag:start_link(beambag_mapping_example,
                                            "./../priv/test.etf",
                                            "./../ebin/beambag_mapping_example.beam",
                                            {raw, fun ?MODULE:build_raw/1}),
             Pid
     end,
     fun(Pid) ->
             ok = gen_server:call(Pid, stop)
     end,
     [ {"last_updated", fun last_updated/0},
       {"bad request", fun bad_req/0},
       {"testing multi edits", fun testing_edits/0}
     ]
    }.

last_updated() ->
    LastUpdated = beambag:last_updated(beambag_mapping_example),
    ?assertMatch({{_Year, _Month, _Day}, {_H, _M, _S}}, LastUpdated),
    ok.


bad_req() ->
    ?assertEqual({error, badreq},
                 gen_server:call(beambag_mapping_example_edit, bad_request)),
    ok.

trigger_edit() ->
    trigger_edit(1000).
trigger_edit(0) ->
    ok;
trigger_edit(N) ->
    %% XXX - This test needs some work
    Now = calendar:local_time(),
    %% Force a new mtime
    ?cmd("touch ../priv/test.etf"),
    ?cmd("touch ../ebin/beambag_mapping_example.beam"),
    Pid = whereis(beambag_mapping_example_edit),
    Pid ! interval,
    case beambag:last_updated(beambag_mapping_example) of
        Now ->
            trigger_edit(N-1);
        _ ->
            ok
    end.

testing_edits() ->
    Data = [{<<"testmap">>, [{<<"test">>, {'test_info', <<"test">>, 'null'}}
                            ]}],
    file:write_file("../priv/test.etf", term_to_binary(Data)),
    trigger_edit(),
    ?assertEqual({term, {1,
                  {<<"test">>,
                  {test_info,<<"test">>,null},
                  nil,nil}}},
                 beambag_mapping_example:state()),
    Data1 = [{<<"testmap">>, [{<<"test">>, {'test_info', <<"test">>, 'null'}},
                              {<<"edit_test">>, {'edit_info', <<"edit">>, 'null'}}
                             ]}],
    file:write_file("../priv/test.etf", term_to_binary(Data1)),
    trigger_edit(),
    ?assertEqual({term, {2,
                  {<<"edit_test">>,
                   {edit_info,<<"edit">>,null},
                   {<<"test">>,{test_info,<<"test">>,null},nil,nil},
                   nil}}},
                 beambag_mapping_example:state()),
    ok.

-module(beampkg_tests).

-include_lib("eunit/include/eunit.hrl").

-include("beambag_edit_magic.hrl").

md5str(Binary) ->
    MD5 = crypto:hash(md5, Binary),
    lists:flatten([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(MD5)]).

check_md5_test() ->
    crypto:start(),
    RBinary = crypto:rand_bytes(200),
    crypto:hash(md5, RBinary),
    FN = "_propadata." ++ md5str(RBinary),
    ok = file:write_file(FN, RBinary),
    ?assertMatch(ok, beampkg:check_md5(FN)),
    file:delete(FN).

check_last_package_test() ->
    ok = file:write_file("f1.abc", <<>>),
    timer:sleep(2000),
    ok = file:write_file("f2.abc", <<>>),
    ?assertMatch("./f2.abc", element(2,beampkg:get_last_package("./f[0-9].abc", test_module))),
    file:delete("f1.abc"),
    file:delete("f2.abc").

complete_test() ->
    [file:delete(F) || F <- filelib:wildcard("propadata.*")],
    file:delete("edited/simple_template.beam"),
    file:delete("edited"),
    {ok, Editor} = beampkg:start_link(simple_template, ".", "propadata.[0-9a-f]*"),
    beampkg:last_updated(simple_template),
    case code:is_loaded(simple_template) of
        {file, _} -> code:delete(simple_template), code:purge(simple_template);
        _ -> do_nothing
    end,
    {ok, MTs, _} = erl_scan:string("-module(simple_template)."),
    {ok, ETs, _} = erl_scan:string("-export([data/0])."),
    {ok, FTs, _} = erl_scan:string("data() -> {'$$magic'}."),
    {ok,MF} = erl_parse:parse_form(MTs),
    {ok,EF} = erl_parse:parse_form(ETs),
    {ok,FF} = erl_parse:parse_form(FTs),
    {ok, simple_template, Bin} = compile:forms([MF,EF,FF]),
    Package = [{data, hello_world}, {template, Bin}, {code_change, fun(_, NewData) -> NewData end}],
    PackageBin = term_to_binary(Package, [compressed]),
    FN = "propadata." ++ md5str(PackageBin),
    ok = file:write_file(FN ++ ".temp", PackageBin),
    ok = file:rename(FN ++ ".temp", FN),
    MTime = beampkg:get_file_mtime(FN),
    Editor ! interval,
    io:format("last_updated: ~p, mtime: ~p~n", [beampkg:last_updated(simple_template), MTime]),
    ?assertMatch(MTime, beampkg:last_updated(simple_template)),
    ?assertMatch(hello_world, simple_template:data()),
    file:delete(FN),
    file:delete("edited/simple_template.beam"),
    file:delete("edited").

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
    FN = "beambag_propadata._module." ++ md5str(RBinary),
    ok = file:write_file(FN, RBinary),
    ?assertMatch(ok, beampkg:check_md5(FN)),
    file:delete(FN).

check_last_package_test() ->
    ok = file:write_file("beambag_propadata.f1.abc", <<>>),
    timer:sleep(1000),
    ok = file:write_file("beambag_propadata.f1.abc2", <<>>),
    ok = file:write_file("beambag_propadata.f2.abc3", <<>>),
    LastPackages = beampkg:get_last_packages("./beambag_propadata.[a-z0-9_]*.[0-9a-f]*"),
    ?assertMatch("./beambag_propadata.f1.abc2", element(2, element(2, dict:find("f1", LastPackages)))),
    ?assertMatch("./beambag_propadata.f2.abc3", element(2, element(2, dict:find("f2", LastPackages)))),
    file:delete("beambag_propadata.f1.abc"),
    file:delete("beambag_propadata.f1.abc2"),
    file:delete("beambag_propadata.f2.abc3").

complete_test() ->
    [file:delete(F) || F <- filelib:wildcard("beambag_propadata.*")],
    file:delete("edited/tmodule.beam"),
    file:del_dir("edited"),
    {ok, Editor} = beampkg:start_link(editor, "."),
    beampkg:last_updated(editor, tmodule),
    case code:is_loaded(tmodule) of
        {file, _} -> code:delete(tmodule), code:purge(tmodule);
        _ -> do_nothing
    end,
    {ok, MTs, _} = erl_scan:string("-module(tmodule)."),
    {ok, ETs, _} = erl_scan:string("-export([data/0])."),
    {ok, FTs, _} = erl_scan:string("data() -> {'$$magic'}."),
    {ok,MF} = erl_parse:parse_form(MTs),
    {ok,EF} = erl_parse:parse_form(ETs),
    {ok,FF} = erl_parse:parse_form(FTs),
    {ok, tmodule, Bin} = compile:forms([MF,EF,FF]),
    Package = [{data, hello_world}, {template, Bin}, {code_change, fun(_, NewData) -> NewData end}, {module, tmodule}],
    PackageBin = term_to_binary(Package, [compressed]),
    FN = "beambag_propadata.tmodule." ++ md5str(PackageBin),
    ok = file:write_file(FN ++ ".temp", PackageBin),
    ok = file:rename(FN ++ ".temp", FN),
    MTime = beambag:get_file_mtime(FN),
    Editor ! interval,
    io:format("last_updated: ~p, mtime: ~p~n", [beampkg:last_updated(editor, tmodule), MTime]),
    ?assertMatch(MTime, beampkg:last_updated(editor, tmodule)),
    ?assertMatch(hello_world, tmodule:data()),
    beampkg:stop(editor),
    file:delete(FN),
    file:delete("edited/tmodule.beam"),
    file:del_dir("edited").

refresh_test() ->
    [file:delete(F) || F <- filelib:wildcard("beambag_propadata.*")],
    file:delete("edited/tmodule.beam"),
    file:del_dir("edited"),
    {ok, Editor} = beampkg:start_link(editor, "."),
    beampkg:last_updated(editor, tmodule),
    case code:is_loaded(tmodule) of
        {file, _} -> code:delete(tmodule), code:purge(tmodule);
        _ -> do_nothing
    end,
    {ok, MTs, _} = erl_scan:string("-module(tmodule)."),
    {ok, ETs, _} = erl_scan:string("-export([data/0])."),
    {ok, FTs, _} = erl_scan:string("data() -> {'$$magic'}."),
    {ok,MF} = erl_parse:parse_form(MTs),
    {ok,EF} = erl_parse:parse_form(ETs),
    {ok,FF} = erl_parse:parse_form(FTs),
    {ok, tmodule, Bin} = compile:forms([MF,EF,FF]),

    Package = [{data, hello_world}, {template, Bin}, {code_change, fun(_, NewData) -> NewData end}, {module, tmodule}],
    PackageBin = term_to_binary(Package, [compressed]),
    FN = "beambag_propadata.tmodule." ++ md5str(PackageBin),
    ok = file:write_file(FN ++ ".temp", PackageBin),
    ok = file:rename(FN ++ ".temp", FN),
    MTime = beambag:get_file_mtime(FN),
    Editor ! interval,
    ?assertMatch(MTime, beampkg:last_updated(editor, tmodule)),
    ?assertMatch(hello_world, tmodule:data()),

    timer:sleep(1000),

    NewPackage = [{data, hello_world_2}, {template, Bin}, {code_change, fun(_, NewData) -> NewData end}, {module, tmodule}],
    NewPackageBin = term_to_binary(NewPackage, [compressed]),
    NewFN = "beambag_propadata.tmodule." ++ md5str(NewPackageBin),
    ok = file:write_file(NewFN ++ ".temp", NewPackageBin),
    ok = file:rename(NewFN ++ ".temp", NewFN),
    NewMTime = beambag:get_file_mtime(NewFN),
    Editor ! interval,
    ?assertNot(NewMTime == MTime),
    ?assertMatch(NewMTime, beampkg:last_updated(editor, tmodule)),
    ?assertMatch(hello_world_2, tmodule:data()),

    beampkg:stop(editor),
    file:delete(FN),
    file:delete(NewFN),
    file:delete("edited/tmodule.beam"),
    file:del_dir("edited").

load_second_test() ->
    [file:delete(F) || F <- filelib:wildcard("beambag_propadata.*")],
    file:delete("edited/tmodule.beam"),
    file:delete("edited/tmodule2.beam"),
    file:del_dir("edited"),
    {ok, Editor} = beampkg:start_link(editor, "."),
    beampkg:last_updated(editor, tmodule),
    case code:is_loaded(tmodule) of
        {file, _} -> code:delete(tmodule), code:purge(tmodule);
        _ -> do_nothing
    end,
    case code:is_loaded(tmodule2) of
        {file, _} -> code:delete(tmodule2), code:purge(tmodule2);
        _ -> do_nothing
    end,

    {ok, MTs, _} = erl_scan:string("-module(tmodule)."),
    {ok, ETs, _} = erl_scan:string("-export([data/0])."),
    {ok, FTs, _} = erl_scan:string("data() -> {'$$magic'}."),
    {ok,MF} = erl_parse:parse_form(MTs),
    {ok,EF} = erl_parse:parse_form(ETs),
    {ok,FF} = erl_parse:parse_form(FTs),
    {ok, tmodule, Bin} = compile:forms([MF,EF,FF]),
    Package = [{data, hello_world}, {template, Bin}, {code_change, fun(_, NewData) -> NewData end}, {module, tmodule}],
    PackageBin = term_to_binary(Package, [compressed]),
    FN = "beambag_propadata.tmodule." ++ md5str(PackageBin),
    ok = file:write_file(FN ++ ".temp", PackageBin),
    ok = file:rename(FN ++ ".temp", FN),
    MTime = beambag:get_file_mtime(FN),
    Editor ! interval,
    ?assertMatch(MTime, beampkg:last_updated(editor, tmodule)),
    ?assertMatch(hello_world, tmodule:data()),

    timer:sleep(1000),

    {ok, MTs2, _} = erl_scan:string("-module(tmodule2)."),
    {ok, ETs2, _} = erl_scan:string("-export([data/0])."),
    {ok, FTs2, _} = erl_scan:string("data() -> {'$$magic'}."),
    {ok,MF2} = erl_parse:parse_form(MTs2),
    {ok,EF2} = erl_parse:parse_form(ETs2),
    {ok,FF2} = erl_parse:parse_form(FTs2),
    {ok, tmodule2, Bin2} = compile:forms([MF2,EF2,FF2]),
    NewPackage = [{data, hello_world_2}, {template, Bin2}, {code_change, fun(_, NewData) -> NewData end}, {module, tmodule2}],
    NewPackageBin = term_to_binary(NewPackage, [compressed]),
    NewFN = "beambag_propadata.tmodule2." ++ md5str(NewPackageBin),
    ok = file:write_file(NewFN ++ ".temp", NewPackageBin),
    ok = file:rename(NewFN ++ ".temp", NewFN),
    NewMTime = beambag:get_file_mtime(NewFN),
    Editor ! interval,
    ?assertNot(NewMTime == MTime),
    ?assertMatch(MTime, beampkg:last_updated(editor, tmodule)),
    ?assertMatch(hello_world, tmodule:data()),
    ?assertMatch(NewMTime, beampkg:last_updated(editor, tmodule2)),
    ?assertMatch(hello_world_2, tmodule2:data()),

    beampkg:stop(editor),
    file:delete(FN),
    file:delete(NewFN),
    file:delete("edited/tmodule.beam"),
    file:delete("edited/tmodule2.beam"),
    file:del_dir("edited").

magic_reloader_test() ->
    code:delete(tmodule),
    code:purge(tmodule),
    file:delete("other_simple_template/tmodule.beam"),
    file:del_dir("other_simple_template"),
    ok = file:make_dir("other_simple_template"),
    {ok, MTs, _} = erl_scan:string("-module(tmodule)."),
    {ok, ETs, _} = erl_scan:string("-export([data/0])."),
    {ok, FTs, _} = erl_scan:string("data() -> other_atom."),
    {ok,MF} = erl_parse:parse_form(MTs),
    {ok,EF} = erl_parse:parse_form(ETs),
    {ok,FF} = erl_parse:parse_form(FTs),
    {ok, tmodule, Bin} = compile:forms([MF,EF,FF]),
    ok = file:write_file("other_simple_template/tmodule.beam", Bin),
    true = code:add_patha("other_simple_template"),
    {module, tmodule} = code:load_file(tmodule),
    other_atom = tmodule:data(),
    complete_test(),
    file:delete("other_simple_template/tmodule.beam"),
    file:del_dir("other_simple_template").


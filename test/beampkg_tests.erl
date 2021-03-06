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
    try
        ok = file:write_file(FN, RBinary),
        ?assertMatch(ok, beampkg:check_md5(FN))
    after
        file:delete(FN)
    end.

check_last_package_test() ->
    try
        ok = file:write_file("beambag_propadata.f1.abc", <<>>),
        timer:sleep(1000),
        ok = file:write_file("beambag_propadata.f1.abc2", <<>>),
        ok = file:write_file("beambag_propadata.f2.abc3", <<>>),
        LastPackages = beampkg:get_last_packages("./beambag_propadata.[a-z0-9_]*.[0-9a-f]*"),
        ?assertMatch("./beambag_propadata.f1.abc2", element(2, element(2, dict:find("f1", LastPackages)))),
        ?assertMatch("./beambag_propadata.f2.abc3", element(2, element(2, dict:find("f2", LastPackages))))
    after
        file:delete("beambag_propadata.f1.abc"),
        file:delete("beambag_propadata.f1.abc2"),
        file:delete("beambag_propadata.f2.abc3")
    end.

binary_to_beam(Binary) ->
    {ok, Ts, _} = erl_scan:string(binary_to_list(Binary)),
    {FsI, _} = lists:foldl(fun({dot,_} = Dot,
                               {Parts, Remaining}) ->
                                   {[lists:reverse([Dot | Remaining]) | Parts], []};
                              (Other, {Parts, Remaining}) ->
                                   {Parts, [Other | Remaining]}
                           end, {[], []}, Ts),
    compile:forms([begin {ok, PFs} = erl_parse:parse_form(F), PFs end
                   || F <- lists:reverse(FsI)]). % {ok, Module, Binary}

unload_module(M) ->
    code:delete(M),
    code:purge(M).

complete_test() ->
    [file:delete(F) || F <- filelib:wildcard("beambag_propadata.*")],
    file:delete("edited/tmodule.beam"),
    file:del_dir("edited"),
    try
        {ok, Editor} = beampkg:start_link(editor, "."),
        beampkg:last_updated(editor, tmodule),
        unload_module(tmodule),
        {ok, tmodule, Bin} = binary_to_beam(<<"-module(tmodule).\n-export([data/0]).\ndata() -> {'$$magic'}.\n">>),
        Package = [{data, hello_world}, {template, Bin}, {code_change, fun(_, NewData) -> NewData end}, {module, tmodule}],
        PackageBin = term_to_binary(Package, [compressed]),
        FN = "beambag_propadata.tmodule." ++ md5str(PackageBin),
        try
            ok = file:write_file(FN ++ ".temp", PackageBin),
            ok = file:rename(FN ++ ".temp", FN),
            MTime = beambag:get_file_mtime(FN),
            Editor ! interval,
            io:format("last_updated: ~p, mtime: ~p~n", [beampkg:last_updated(editor, tmodule), MTime]),
            ?assertMatch(MTime, beampkg:last_updated(editor, tmodule)),
            ?assertMatch(hello_world, tmodule:data())
        after
            beampkg:stop(editor),
            file:delete(FN)
        end
    after
        file:delete("edited/tmodule.beam"),
        file:del_dir("edited")
    end.

refresh_test() ->
    [file:delete(F) || F <- filelib:wildcard("beambag_propadata.*")],
    file:delete("edited/tmodule.beam"),
    file:del_dir("edited"),
    try
        {ok, Editor} = beampkg:start_link(editor, "."),
        beampkg:last_updated(editor, tmodule),
        unload_module(tmodule),
        {ok, tmodule, Bin} = binary_to_beam(<<"-module(tmodule).\n-export([data/0]).\ndata() -> {'$$magic'}.\n">>),

        Package = [{data, hello_world}, {template, Bin}, {code_change, fun(_, NewData) -> NewData end}, {module, tmodule}],
        PackageBin = term_to_binary(Package, [compressed]),
        FN = "beambag_propadata.tmodule." ++ md5str(PackageBin),
        try
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
            try
                Editor ! interval,
                ?assertNot(NewMTime == MTime),
                ?assertMatch(NewMTime, beampkg:last_updated(editor, tmodule)),
                ?assertMatch(hello_world_2, tmodule:data())

            after
                beampkg:stop(editor),
                file:delete(NewFN)
            end
        after
            file:delete(FN)
        end
    after
        file:delete("edited/tmodule.beam"),
        file:del_dir("edited")
    end.

load_second_test() ->
    [file:delete(F) || F <- filelib:wildcard("beambag_propadata.*")],
    file:delete("edited/tmodule.beam"),
    file:delete("edited/tmodule2.beam"),
    file:del_dir("edited"),
    {ok, Editor} = beampkg:start_link(editor, "."),
    beampkg:last_updated(editor, tmodule),
    unload_module(tmodule),
    unload_module(tmodule2),

    try
        {ok, tmodule, Bin} = binary_to_beam(<<"-module(tmodule).\n-export([data/0]).\ndata() -> {'$$magic'}.\n">>),
        Package = [{data, hello_world}, {template, Bin}, {code_change, fun(_, NewData) -> NewData end}, {module, tmodule}],
        PackageBin = term_to_binary(Package, [compressed]),
        FN = "beambag_propadata.tmodule." ++ md5str(PackageBin),
        try
            ok = file:write_file(FN ++ ".temp", PackageBin),
            ok = file:rename(FN ++ ".temp", FN),
            MTime = beambag:get_file_mtime(FN),
            Editor ! interval,
            ?assertMatch(MTime, beampkg:last_updated(editor, tmodule)),
            ?assertMatch(hello_world, tmodule:data()),

            timer:sleep(1000),

            {ok, tmodule2, Bin2} = binary_to_beam(<<"-module(tmodule2).\n-export([data/0]).\ndata() -> {'$$magic'}.\n">>),
            NewPackage = [{data, hello_world_2}, {template, Bin2}, {code_change, fun(_, NewData) -> NewData end}, {module, tmodule2}],
            NewPackageBin = term_to_binary(NewPackage, [compressed]),
            NewFN = "beambag_propadata.tmodule2." ++ md5str(NewPackageBin),
            try
                ok = file:write_file(NewFN ++ ".temp", NewPackageBin),
                ok = file:rename(NewFN ++ ".temp", NewFN),
                NewMTime = beambag:get_file_mtime(NewFN),
                Editor ! interval,
                ?assertNot(NewMTime == MTime),
                ?assertMatch(MTime, beampkg:last_updated(editor, tmodule)),
                ?assertMatch(hello_world, tmodule:data()),
                ?assertMatch(NewMTime, beampkg:last_updated(editor, tmodule2)),
                ?assertMatch(hello_world_2, tmodule2:data()),

                beampkg:stop(editor)
            after
                file:delete(NewFN)
            end
        after
            file:delete(FN)
        end
    after
        file:delete("edited/tmodule.beam"),
        file:delete("edited/tmodule2.beam"),
        file:del_dir("edited")
    end.

magic_reloader_test() ->
    unload_module(tmodule),
    file:delete("other_simple_template/tmodule.beam"),
    file:del_dir("other_simple_template"),
    try
        ok = file:make_dir("other_simple_template"),
        {ok, tmodule, Bin} = binary_to_beam(<<"-module(tmodule).\n-export([data/0]).\ndata() -> other_atom.\n">>),
        ok = file:write_file("other_simple_template/tmodule.beam", Bin),
        true = code:add_patha("other_simple_template"),
        {module, tmodule} = code:load_file(tmodule),
        other_atom = tmodule:data(),
        complete_test()
    after
        file:delete("other_simple_template/tmodule.beam"),
        file:del_dir("other_simple_template")
    end.

restore_and_dict_test() ->
    [file:delete(F) || F <- filelib:wildcard("beambag_propadata.*")],
    file:delete("edited/tmodule.beam"),
    file:del_dir("edited"),
    try
        {ok, Editor} = beampkg:start_link(editor, "."),
        beampkg:last_updated(editor, tmodule),
        unload_module(tmodule),
        {ok, tmodule, Bin} = binary_to_beam(<<"-module(tmodule).\n-export([data/0]).\ndata() -> {'$$magic'}.\n">>),

        Package = [{data, hello_world}, {template, Bin}, {code_change, fun(_, NewData) -> NewData end}, {module, tmodule}],
        PackageBin = term_to_binary(Package, [compressed]),
        FN = "beambag_propadata.tmodule." ++ md5str(PackageBin),
        try
            ok = file:write_file(FN ++ ".temp", PackageBin),
            ok = file:rename(FN ++ ".temp", FN),
            MTime = beambag:get_file_mtime(FN),
            Editor ! interval,
            ?assertMatch(MTime, beampkg:last_updated(editor, tmodule)),
            ?assertMatch(hello_world, tmodule:data()),
            beampkg:stop(editor),
            timer:sleep(1000),
            ?assertMatch(undefined, whereis(editor)),

            NewPackage = [{data, hello_world_2}, {template, Bin}, {code_change, fun(_, NewData) -> NewData end}, {module, tmodule}],
            NewPackageBin = term_to_binary(NewPackage, [compressed]),
            NewFN = "beambag_propadata.tmodule." ++ md5str(NewPackageBin),

            try
                {ok, Editor2} = beampkg:start_link(editor, "."),
                beampkg:last_updated(editor, tmodule),
                ?assertMatch(MTime, beampkg:last_updated(editor, tmodule)),
                ?assertMatch(hello_world, tmodule:data()),

                ok = file:write_file(NewFN ++ ".temp", NewPackageBin),
                ok = file:rename(NewFN ++ ".temp", NewFN),
                Editor2 ! interval,
                NewMTime = beambag:get_file_mtime(NewFN),

                ?assertNot(NewMTime == MTime),
                ?assertMatch(NewMTime, beampkg:last_updated(editor, tmodule)),
                ?assertMatch(hello_world_2, tmodule:data())

            after
                beampkg:stop(editor),
                file:delete(NewFN)
            end
        after
            file:delete(FN)
        end
    after
        file:delete("edited/tmodule.beam"),
        file:del_dir("edited")
    end.

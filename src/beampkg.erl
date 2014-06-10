%% @author Matthew Dempsky <matthew@mochimedia.com> and Igor Milyakov <virtan@virtan.com>
%% @copyright 2009-2014 Mochi Media, Inc.
%%
%% @doc Module and process for editing beam files
%% for use as a quasi-static in memory data store.

-module(beampkg).

-include_lib("kernel/include/file.hrl").
-include("beambag_edit_magic.hrl").

-behaviour(gen_server).

-export([child_spec/2, start_link/2, last_updated/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([check_md5/1, get_last_packages/1]).

-record(beampkg_state, {wildcard, modules, edited_beams_dir, tref}).

%% External API
%% @type buildfun() = {raw, (FileName::string()) -> term()} |
%%                    ([Data::term()]) -> term().

-spec child_spec(atom(), string()) -> list().
child_spec(EditorName, PackageDir) ->
    [{EditorName,
     {?MODULE, start_link, [EditorName, PackageDir]},
     permanent, 5000, worker, [EditorName]}, [beampkg]].

%%
%% @doc Start a <code>beampkg</code> server.
-spec start_link(atom(), string()) -> pid().
start_link(EditorName, PackageDir) ->
    gen_server:start_link({local, EditorName}, ?MODULE, [PackageDir], []).

%% @doc Stop the <code>beampkg</code> server.
-spec stop(atom()) -> ok.
stop(EditorName) ->
    gen_server:call(EditorName, stop).

%%
%% @doc Get the date and time when the <code>TargetModule</code> was
%% last updated.
-spec last_updated(atom(), atom()) -> calendar:date_time().
last_updated(EditorName, TargetModule) ->
    gen_server:call(EditorName, {last_updated, TargetModule}).

%% @private
init([PackageDir]) ->
    BaseDir = beambag:get_base_dir(?MODULE),
    PackageFullDir = beambag:full_path(PackageDir, BaseDir),
    PackageFullWildcard = filename:join([PackageFullDir, "beambag_propadata.[a-z0-9_]*.[0-9a-f]*"]),
    EditedBeamsDir = filename:join([BaseDir, "edited"]),
    ok = filelib:ensure_dir(EditedBeamsDir ++ "/"),
    true = code:add_patha(EditedBeamsDir),
    LastPackages = get_last_packages(PackageFullWildcard),
    Packages = dict:map(fun(TargetModuleString, {PackageMTime, PackageFile}) ->
                RunningModuleFile = filename:join([EditedBeamsDir, TargetModuleString ++ ".beam"]),
                case need_edit(RunningModuleFile, TargetModuleString, PackageMTime) of
                    true ->
                        edit(PackageFile, RunningModuleFile);
                    false ->
                        do_nothing
                end,
                {beambag:get_max_mtime([RunningModuleFile, PackageFile]), PackageFile}
             end, LastPackages),
    Modules = lists:foldl(fun(RunningModuleFile, Modules1) ->
                    RunningModule = module_name_from_beam(RunningModuleFile),
                    code:ensure_loaded(list_to_atom(RunningModule)),
                    case dict:is_key(RunningModule, Modules1) of
                        true -> Modules1;
                        false ->
                            dict:store(RunningModule,
                                       {beambag:get_file_mtime(RunningModuleFile), undefined}, Modules1)
                    end
                end, Packages, filelib:wildcard(filename:join([EditedBeamsDir, "*.beam"]))),
    TRef = erlang:send_after(timer:seconds(5), self(), interval),
    State = #beampkg_state{wildcard = PackageFullWildcard,
                           modules = Modules,
                           edited_beams_dir = EditedBeamsDir,
                           tref = TRef},
    {ok, State}.

%% @private
get_last_packages(Wildcard) ->
    FileInfos = lists:foldl(fun(F, Dict) ->
                                    {TargetModule, _} = parse_package_file_name(F),
                                    dict:append(TargetModule, {beambag:get_file_mtime(F), F}, Dict)
                            end, dict:new(), filelib:wildcard(Wildcard)),
    dict:map(fun(TargetModuleString, DataList) ->
                     case length(DataList) of
                         N when N > 10 ->
                             beambag:error_report([{reason,
                                    {"too many packages, please clean up (just a warning, keeping working)"}},
                                    {module, TargetModuleString}, {number_of_packages, N}]);
                         _ -> do_nothing
                     end,
                     case DataList of
                         [] -> {-1, undefined};
                         _ -> lists:max(DataList)
                     end
             end, FileInfos).

%% @private
handle_call({last_updated, TargetModule}, _From, State = #beampkg_state{modules = Modules}) ->
    ModuleString = atom_to_list(TargetModule),
    Res = case dict:find(ModuleString, Modules) of
        error -> -1;
        {ok, {MTime, _}} -> MTime
    end,
    {reply, Res, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badreq}, State}.

%% @private
handle_cast(_Req, State) ->
    {noreply, State}.

%% @private
handle_info(interval, State=#beampkg_state{wildcard = Wildcard, modules = Modules, edited_beams_dir = EditedBeamsDir}) ->
    LastPackages = get_last_packages(Wildcard),
    NewModules = case get_updated_packages(Modules, LastPackages) of
        [] -> Modules;
        UpdatedPackages when is_list(UpdatedPackages) ->
            lists:foldl(fun({NewPackageMTime, NewPackageFile}, Modules1) ->
                                {ModuleNameString, _} = parse_package_file_name(NewPackageFile),
                                RunningModuleFile = filename:join([EditedBeamsDir, ModuleNameString ++ ".beam"]),
                                edit(NewPackageFile, RunningModuleFile),
                                dict:store(ModuleNameString, {NewPackageMTime, NewPackageFile}, Modules1)
                      end, Modules, UpdatedPackages)
    end,
    TRef = erlang:send_after(timer:seconds(5), self(), interval),
    {noreply, State#beampkg_state{modules = NewModules, tref = TRef}};
handle_info(_Req, State) ->
    {noreply, State}.

%% @private
terminate(_Rsn, _State) ->
    ok.

%% @private
code_change(_Vsn, State, _Extra) ->
    {ok, State}.

get_updated_packages(Old, New) ->
    dict:fold(fun(ModuleNameString, {NewPackageMTime, NewPackageFile}, Updated) ->
                      case dict:find(ModuleNameString, Old) of
                          {ok, {RunningModuleMTime, _}} when
                                NewPackageMTime =/= -1 andalso NewPackageMTime > RunningModuleMTime ->
                              [{NewPackageMTime, NewPackageFile} | Updated];
                          {ok, _} -> Updated;
                          error -> [{NewPackageMTime, NewPackageFile} | Updated]
                      end
              end, [], New).

parse_package_file_name(PackageFile) ->
    ["beambag_propadata", ModuleNameString, PackageMD5] = string:tokens(filename:basename(PackageFile), "."),
    {ModuleNameString, PackageMD5}.

module_name_from_beam(BeamFile) ->
    case lists:reverse(filename:basename(BeamFile)) of
        "maeb." ++ ModuleNameStringReversed -> lists:reverse(ModuleNameStringReversed);
        _ -> []
    end.

need_edit(RunningModuleFile, TargetModuleString, PackageMTime) ->
    is_package_newer(RunningModuleFile, PackageMTime) orelse
	beambag:does_the_module_contain_the_magic_marker(list_to_atom(TargetModuleString),
            fun() -> need_edit(RunningModuleFile, TargetModuleString, PackageMTime) end).

is_package_newer(RunningModuleFile, PackageMTime) ->
    RunningModuleMTime = beambag:get_file_mtime(RunningModuleFile),
    PackageMTime > RunningModuleMTime.

edit(undefined, _RunningModuleFile) -> do_nothing;
edit(PackageFile, RunningModuleFile) ->
    try
        Package = load_package(PackageFile),
        case {load_data(Package), proplists:get_value(template, Package)} of
            {{error, Reason}, _} ->
                beambag:error_report([{reason, {"can't load data", Reason}}, {package_file, PackageFile}]);
            {{ok, Data}, NewTemplate} ->
                BeamData = beambag_edit:swap(NewTemplate, ?MAGIC, term_to_binary(Data)),
                TmpFile = RunningModuleFile ++ ".tmp",
                ok = file:write_file(TmpFile, BeamData),
                ok = file:rename(TmpFile, RunningModuleFile),
                c:l(proplists:get_value(module, Package))
        end,
        erlang:garbage_collect(),
        beambag:info_report([{module, proplists:get_value(module, Package)}, {package_file, PackageFile}])
    catch
        Type:Error ->
            beambag:error_report([{reason, {Type, Error}}, {package_file, PackageFile}, {stacktrace, erlang:get_stacktrace()}])
    end.

load_package(PackageFile) ->
    ok = check_md5(PackageFile),
    {ok, Binary} = file:read_file(PackageFile),
    binary_to_term(Binary).

check_md5(PackageFile) ->
    {_, MD5Str} = parse_package_file_name(PackageFile),
    case file:read_file(PackageFile) of
        {ok, Binary} ->
            case {hexstr_to_bin(MD5Str), crypto:hash(md5, Binary)} of
                {Same, Same} -> ok;
                _ -> incorrect_md5
            end;
        _ -> incorrect_md5
    end.

hexstr_to_bin(S) ->
      hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
      list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
      {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
        hexstr_to_bin(T, [V | Acc]).

load_data(Package) ->
    case proplists:get_value(code_change, Package) of
        undefined ->
            case proplists:get_value(data, Package) of
                undefined -> {error, no_data};
                Data1 -> {ok, Data1}
            end;
        CodeChangeF ->
            try
                {ok, CodeChangeF(proplists:get_value(module, Package), proplists:get_value(data, Package))}
            catch
                Type:Error -> {error, {Type, Error}}
            end
    end.


%% @author Matthew Dempsky <matthew@mochimedia.com> and Igor Milyakov <virtan@virtan.com>
%% @copyright 2009-2014 Mochi Media, Inc.
%%
%% @doc Module and process for editing beam files
%% for use as a quasi-static in memory data store.

-module(beampkg).

-include_lib("kernel/include/file.hrl").
-include("beambag_edit_magic.hrl").

-behaviour(gen_server).

-export([child_spec/3, start_link/3, last_updated/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([check_md5/1, get_last_package/2, get_file_mtime/1, error_report/1]).

-record(beampkg_state, {wildcard, current_package, mtime, target, template, module, tref}).

%% External API
%% @type buildfun() = {raw, (FileName::string()) -> term()} |
%%                    ([Data::term()]) -> term().

-spec child_spec(atom(), string(), string()) ->
                        list().
child_spec(TargetModule, PackageDir, PackageWildcard) ->
    EditorName = editor_name(TargetModule),
    [{EditorName,
     {?MODULE, start_link, [TargetModule, PackageDir, PackageWildcard]},
     permanent, 5000, worker, [EditorName]}, [beampkg]].

%%
%% @doc Start a <code>beampkg</code> server.
-spec start_link(atom(), string(), string()) ->
                        pid().
start_link(TargetModule, PackageDir, PackageWildcard) ->
    gen_server:start_link({local, editor_name(TargetModule)},
                          ?MODULE,
                          [TargetModule, PackageDir, PackageWildcard], []).

stop(TargetModule) ->
    EditorName = editor_name(TargetModule),
    gen_server:call(EditorName, stop).

%%
%% @doc Get the date and time when the <code>TargetModule</code> was
%% last updated.
-spec last_updated(atom()) ->
                          calendar:date_time().
last_updated(TargetModule) ->
    EditorName = editor_name(TargetModule),
    gen_server:call(EditorName, last_updated).

%% @private
init([TargetModule, PackageDir, PackageWildcard]) ->
    BaseDir = get_base_dir(?MODULE),
    PackageFullDir = full_path(PackageDir, BaseDir),
    PackageFullWildcard = filename:join([PackageFullDir, PackageWildcard]),
    Target = filename:join([BaseDir, "edited", atom_to_list(TargetModule) ++ ".beam"]),
    ok = filelib:ensure_dir(Target),
    true = code:add_patha(filename:dirname(Target)),
    {_, PackageFile} = get_last_package(PackageFullWildcard, TargetModule),
    MaxMTime = get_max_mtime([PackageFile, Target]),
    State = #beampkg_state{wildcard = PackageFullWildcard,
                   current_package = PackageFile,
                   mtime = MaxMTime,
                   target = Target,
                   template = undefined,
                   module = TargetModule},

    NewState = case need_edit(State) of
        true ->
	    edit(State);
	false ->
	    State
    end,

    TRef = erlang:send_after(timer:seconds(5), self(), interval),
    {ok, NewState#beampkg_state{tref = TRef}}.

%% @private
full_path(Path, BaseDir) ->
    case filename:pathtype(Path) of
        absolute -> Path;
        _ -> filename:join([BaseDir, Path])
    end.

%% @private
%% @doc Return the application directory for Module. It assumes Module is in
%%      a standard OTP layout application in the ebin or src directory.
get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).

%% @private
get_file_mtime(undefined) -> -1;
get_file_mtime(FileName) ->
    case file:read_file_info(FileName) of
        {ok, FileInfo} ->
            file:read_file_info(FileName),
            FileInfo#file_info.mtime;
        {error,enoent} ->
            -1
    end.

%% @private
get_max_mtime(FileNames) ->
    lists:max([get_file_mtime(F) || F <- FileNames]).

%% @private
get_last_package(Wildcard, Module) ->
    FileInfos = [{get_file_mtime(F), F} || F <- filelib:wildcard(Wildcard)],
    case length(FileInfos) of
        N when N > 10 ->
            error_report([{reason, {"too many packages, please clean up", N, "just a warning, keeping working"}}, {module, Module}]);
        _ -> do_nothing
    end,
    case FileInfos of
        [] -> {-1, undefined};
        _ -> lists:max(FileInfos)
    end.

%% @private
handle_call(last_updated, _From, State) ->
    MTime = State#beampkg_state.mtime,
    {reply, MTime, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badreq}, State}.

%% @private
handle_cast(_Req, State) ->
    {noreply, State}.

%% @private
handle_info(interval, State=#beampkg_state{wildcard=Wildcard, module=Module, mtime=OldMTime}) ->
    {_, PackageFile} = get_last_package(Wildcard, Module),
    MTime = get_file_mtime(PackageFile),
    NewState = case MTime =/= -1 andalso MTime > OldMTime of
                   true ->
                       NewState1 = State#beampkg_state{current_package = PackageFile, mtime = MTime},
                       edit(NewState1);
                   false ->
                       State
               end,
    TRef = erlang:send_after(timer:seconds(5), self(), interval),
    NewState3 = NewState#beampkg_state{tref = TRef},
    {noreply, NewState3};
handle_info(_Req, State) ->
    {noreply, State}.

%% @private
terminate(_Rsn, _State) ->
    ok.

%% @private
code_change(_Vsn, State, _Extra) ->
    {ok, State}.

need_edit(State) ->
    is_package_newer(State) orelse
	does_the_module_contain_the_magic_marker(State).

is_package_newer(State) ->
    PackageFileMTime = get_file_mtime(State#beampkg_state.current_package),
    TargetMTime = get_file_mtime(State#beampkg_state.target),
    PackageFileMTime > TargetMTime.

does_the_module_contain_the_magic_marker(State) ->
    Module = State#beampkg_state.module,
    %% Sometimes mtime isn't enough. We want to make sure the source
    %% data is edited into the target beam.
    case {code:is_loaded(Module), code:get_object_code(Module)} of
        {false, _} -> false;
        {{file, Filename}, {Module, Beam, Filename}} ->
	    %% Check for magic.
	    beambag_edit:has_magic(Beam, ?MAGIC);
        {{file, _Filename}, {Module, _Beam, _OtherFilename}} ->
	    %% Load target beam if not used.
	    code:purge(Module),
	    code:load_file(Module),
	    need_edit(State);
        {_, error} ->
	    false
    end.

edit(State = #beampkg_state{current_package = undefined}) -> State;
edit(State = #beampkg_state{current_package = PackageFile, module = Module, template = Template}) ->
    try
        Package = load_package(PackageFile),
        case {load_data(Module, Package), proplists:get_value(template, Package, Template)} of
            {{error, Reason}, _} ->
                error_report([{reason, {"can't load data", Reason}}, {package_file, PackageFile}, {module, Module}]),
                State;
            {{ok, _}, undefined} ->
                error_report([{reason, "template is missing"}, {package_file, PackageFile}, {module, Module}]),
                State;
            {{ok, Data}, NewTemplate} ->
                BeamData = beambag_edit:swap(NewTemplate, ?MAGIC, term_to_binary(Data)),
                TmpFile = State#beampkg_state.target ++ ".tmp",
                ok = file:write_file(TmpFile, BeamData),
                ok = file:rename(TmpFile, State#beampkg_state.target),
                c:l(State#beampkg_state.module),
                erlang:garbage_collect(),
                State#beampkg_state{template = NewTemplate}
        end
    catch
        Type:Error ->
            error_report([{reason, {Type, Error}}, {package_file, PackageFile}, {module, Module}, {stacktrace, erlang:get_stacktrace()}]),
            State

    end.

load_package(PackageFile) ->
    ok = check_md5(PackageFile),
    {ok, Binary} = file:read_file(PackageFile),
    binary_to_term(Binary).

check_md5(PackageFile) ->
    MD5Str = lists:filter(fun(S) when length(S) == 32 ->
                                  case re:run(S, "^[0-9a-f]+$") of nomatch -> false; _ -> true end;
                             (_) -> false end,
                          string:tokens(PackageFile, ".")),
    case {file:read_file(PackageFile), MD5Str} of
        {{ok, Binary}, [MD5Str1]} ->
            case {hexstr_to_bin(MD5Str1), crypto:hash(md5, Binary)} of
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

load_data(Module, Package) ->
    case proplists:get_value(code_change, Package) of
        undefined ->
            case proplists:get_value(data, Package) of
                undefined -> {error, no_data};
                Data1 -> {ok, Data1}
            end;
        CodeChangeF ->
            try
                {ok, CodeChangeF(Module, proplists:get_value(data, Package))}
            catch
                Type:Error -> {error, {Type, Error}}
            end
    end.

editor_name(TargetModule) ->
    list_to_atom(atom_to_list(TargetModule) ++ "_edit").

error_report(PList) ->
    error_logger:warning_report(["can't load package, skipping beamedit"] ++ PList).

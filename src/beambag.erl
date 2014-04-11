%% @author Matthew Dempsky <matthew@mochimedia.com>
%% @copyright 2009-2014 Mochi Media, Inc.
%%
%% @doc Module and process for editing beam files
%% for use as a quasi-static in memory data store.

-module(beambag).

-include_lib("kernel/include/file.hrl").
-include("beambag_edit_magic.hrl").

-behaviour(gen_server).

-export([child_spec/4, start_link/4, last_updated/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(beambag_state, {file, mtime, template, target, module, tref, buildfun}).


%% External API
%% @type buildfun() = {raw, (FileName::string()) -> term()} |
%%                    ([Data::term()]) -> term().

-spec child_spec(atom(), string(), string(), fun()) ->
                        list().
child_spec(TargetModule, SourceFile, Template, BuildFun) ->
    EditorName = editor_name(TargetModule),
    [{EditorName,
     {?MODULE, start_link, [TargetModule, SourceFile, Template, BuildFun]},
     permanent, 5000, worker, [EditorName]}, [beambag]].

%%
%% @doc Start a <code>beambag</code> server.
-spec start_link(atom(), string(), string(), fun()) ->
                        pid().
start_link(TargetModule, SourceFile, Template, BuildFun) ->
    gen_server:start_link({local, editor_name(TargetModule)},
                          ?MODULE,
                          [TargetModule, SourceFile, Template, BuildFun], []).

%%
%% @doc Get the date and time when the <code>TargetModule</code> was
%% last updated.
-spec last_updated(atom()) ->
                          calendar:date_time().
last_updated(TargetModule) ->
    EditorName = editor_name(TargetModule),
    gen_server:call(EditorName, last_updated).

%% @private
init([TargetModule, SF, T, BuildFun]) ->
    BaseDir = get_base_dir(?MODULE),
    Template = full_path(T, BaseDir),
    SourceFile = full_path(SF, BaseDir),
    Target = filename:join([filename:dirname(filename:dirname(Template)),
                            "edit", filename:basename(Template)]),
    ok = filelib:ensure_dir(Target),
    true = code:add_patha(filename:dirname(Target)),
    MaxMTime = get_max_mtime([SourceFile, Template]),
    State = #beambag_state{file = SourceFile,
                   mtime = MaxMTime,
                   template = Template, target = Target,
                   module = TargetModule,
                   buildfun = BuildFun},

    case need_edit(State) of
        true ->
	    ok = edit(State);
	false ->
	    ok
    end,

    {ok, TRef} = timer:send_after(timer:seconds(5), interval),
    {ok, State#beambag_state{tref = TRef}}.

%% @private
full_path("/" ++ Rest, _BaseDir) ->
    "/" ++ Rest;
full_path(Path, BaseDir) ->
    BaseDir ++ "/" ++ Path.

%% @private
%% @doc Return the application directory for Module. It assumes Module is in
%%      a standard OTP layout application in the ebin or src directory.
get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).

%% @private
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
handle_call(last_updated, _From, State) ->
    MTime = State#beambag_state.mtime,
    {reply, MTime, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badreq}, State}.

%% @private
handle_cast(_Req, State) ->
    {noreply, State}.

%% @private
handle_info(interval, State=#beambag_state{file=File, template=Template,
                                   mtime=OldMTime}) ->
    {Time, State1} =
        try
            MTime = get_max_mtime([File, Template]),
            case MTime =/= OldMTime of
                true ->
                    edit(State),
                    ok;
                false ->
                    ok
            end,
            {timer:seconds(5), State#beambag_state{mtime = MTime}}
        catch
            Type:What ->
                %% Send error event
                error_logger:error_report(
                  ["problem editing beam",
                   {file, State#beambag_state.file},
                   {template, State#beambag_state.template},
                   {target, State#beambag_state.target},
                   {type, Type},
                   {what, What},
                   {tb, erlang:get_stacktrace()}]),
                {timer:minutes(1), State}
        end,
    {ok, TRef} = timer:send_after(Time, interval),
    NewState = State1#beambag_state{tref = TRef},
    {noreply, NewState};
handle_info(_Req, State) ->
    {noreply, State}.

%% @private
terminate(_Rsn, _State) ->
    ok.

%% @private
code_change(_Vsn, State, _Extra) ->
    {ok, State}.

need_edit(State) ->
    is_template_newer(State) orelse
	does_the_module_contain_the_magic_marker(State).

is_template_newer(State) ->
    TemplateMTime = get_file_mtime(State#beambag_state.template),
    TargetMTime = get_file_mtime(State#beambag_state.target),
    TemplateMTime > TargetMTime.

does_the_module_contain_the_magic_marker(State) ->
    Module = State#beambag_state.module,
    {file, Filename} = code:is_loaded(Module),
    %% Sometimes mtime isn't enough. We want to make sure the source
    %% data is edited into the target beam.
    case code:get_object_code(Module) of
	{Module, Beam, Filename} ->
	    %% Check for magic.
	    beambag_edit:has_magic(Beam, ?MAGIC);
	{Module, _Beam, _OtherFilename} ->
	    %% Load target beam if not used.
	    code:load_file(Module),
	    need_edit(State);
	error ->
	    false
    end.

edit(State) ->
    Data = getbinarydata(State#beambag_state.file, State#beambag_state.buildfun),
    case Data of
        error ->
            error_logger:warning_msg("Data is invalid, skipping beamedit.~n"),
            ok;
        _ ->
            {ok, BeamTemplate} = file:read_file(State#beambag_state.template),
            BeamData = beambag_edit:swap(BeamTemplate, ?MAGIC, Data),
            TmpFile = State#beambag_state.target ++ ".tmp",
            ok = file:write_file(TmpFile, BeamData),
            ok = file:rename(TmpFile, State#beambag_state.target),
            c:l(State#beambag_state.module),
            erlang:garbage_collect(),
            ok
    end.

getbinarydata(FileName, Builder) ->
    case getdata(FileName, Builder) of
	{_, error = Error} ->
	    Error;
	{binary, Data} ->
	    Data;
	{term, Data} ->
	    term_to_binary(Data)
    end.

getdata(FileName, {binary, BuildFun}) ->
    {binary, BuildFun(FileName)};
getdata(FileName, {raw, BuildFun}) ->
    {term, BuildFun(FileName)};
getdata(FileName, BuildFun) ->
    {ok, Data} = file:consult(FileName),
    {term, BuildFun(Data)}.

editor_name(TargetModule) ->
    list_to_atom(atom_to_list(TargetModule) ++ "_edit").

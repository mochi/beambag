-module(beambag_listen).

-behaviour(gen_server).

-export([child_spec/4, start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(beambag_listen_state, {file, mtime, template, target, module, tref, buildfun}).


%% @doc - Child specification for creating a beambag listener.
%% XXX - have a pool of listeners?
%% XXX - Still needs some thought and work
-spec child_spec(atom(), list(fun())) ->
                        list().
child_spec(ListenerName, Listeners) ->
    [{ListenerName,
     {?MODULE, start_link, [ListenerName, Listeners]},
     permanent, 5000, worker, [ListenerName]}, [beambag_listen]].

%%
%% @doc Start a <code>beambag_listen</code> server.
-spec start_link(atom(), list(fun())) ->
                        pid().
start_link(ListnerName, Listeners) ->
    gen_server:start_link({local, ListenerName},
                          ?MODULE, [Listeners], []).


handle_call(_Req, _From, State) ->
    {reply, {error, badreq}, State}.

%% @private
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Req, State) ->
    {noreply, State}.

%% @private
terminate(_Rsn, _State) ->
    ok.

%% @private
code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%% @private
init([]) ->
    ok.



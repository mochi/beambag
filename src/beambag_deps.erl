%% @copyright 2010-2014 Mochi Media, Inc.

%% @doc Ensure that the dependencies are on the code path.
-module(beambag_deps).

-export([ensure/0,
         get_base_dir/0,
         get_base_dir/1,
         local_path/1,
         local_path/2]).

%% @doc Ensure that the ebin and include paths for dependencies of
%%      this application are on the code path.
-spec ensure() -> ok.
ensure() ->
    {ok, Cwd} = file:get_cwd(),
    code:add_paths(deps_on_path(Cwd)),
    code:clash(),
    ok.

%% @doc Find all ebin and include dirs that are Base ++ "/*/{ebin,include}".
-spec deps_on_path(string()) -> list(string()).
deps_on_path(Base) ->
    filelib:wildcard(filename:join([Base, "*", "{ebin,include}"])).

%% @doc Return the application directory for Module. It assumes Module is in
%%      a standard OTP layout application in the ebin or src directory.
-spec get_base_dir(atom()) -> string().
get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).

%% @doc Return the application directory for this application. Equivalent to
%%      get_base_dir(?MODULE).
-spec get_base_dir() -> string().
get_base_dir() ->
    get_base_dir(?MODULE).

%% @doc Return an application-relative directory from Module's application.
-spec local_path([string()], atom()) -> string().
local_path(Components, Module) ->
    filename:join([get_base_dir(Module) | Components]).

%% @doc Return an application-relative directory for this application.
%%      Equivalent to local_path(Components, ?MODULE).
-spec local_path(list()) -> string().
local_path(Components) ->
    local_path(Components, ?MODULE).


%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% The {@module} module provides functions that interact with the ERTS to
%%% return information relating to Erlang 'applications' in the system.
%%%
%%% === Types ===
%%%
%%% The {@module} module exports several types ({@section app_tpl},
%%% {@section app_nfo}, {@section app_rec}) that are capable of representing
%%% ERTS application metadata.
%%%
%%% ==== app_tpl ====
%%%
%%% The native 3-tuple returned from ERTS module function calls.
%%%
%%% Example:
%%%   ```
%%%   {kernel,"ERTS  CXC 138 10","3.0.1"}
%%%   '''
%%%
%%% ==== app_nfo ====
%%%
%%% A 'map' based representation where the 'description' and 'version' values
%%% are 'binaries'.
%%%
%%% Suitable for conversion to hierarchical serializable representations
%%% such as 'json', 'xml', etc.
%%%
%%% Example:
%%%   ```
%%%   #{app_nfo => #{
%%%     description => <<"ERTS  CXC 138 10">>,
%%%     name => kernel,
%%%     version => <<"3.0.1">>
%%%     }
%%%   }
%%%   '''
%%%
%%% ==== app_rec ====
%%%
%%% A 'record' based representation where the 'description' and 'version'
%%% values are 'binaries'.
%%%
%%% Example:
%%%   ```
%%%   #{app_rec{kernel,<<"ERTS  CXC 138 10">>,<<"3.0.1">>}}
%%%   '''
%%% @end
%%%----------------------------------------------------------------------------
-module(erts_apps).
-author("Temple").

%%% Export all declared functions when TEST.
-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
  -compile(export_all).
-endif.

%%%============================================================================
%%% Public API
%%%============================================================================

-export_type([
  app_tpl/0,                   % Erlang 'app' tuple type.
  app_nfo/0,                   % Erlang 'app' map type.
  app_start_res/0,             % start_apps/2 result type.
  app_stop_res/0               % stop_apps/2 result type.
]).

-export([
  get_loaded_apps/0,           % Get the currently 'loaded' apps.
  get_running_apps/0,          % Get the currently 'running' apps.
  start_apps/1,                % Start the specified app/apps.
  stop_apps/1,                 % Stop the specified app/apps.
  get_app_cnfg/1,              % Get the configuration of the specified app.
  to_map/1,                    % Convert an app_tpl to an app_nfo.
  to_record/1                  % Convert an app_tpl to an app_rec.
]).


%%%============================================================================
%%% Public Types
%%%============================================================================

%%-----------------------------------------------------------------------------
%% A type definition defining the 3-tuple ERTS representation of an
%% application.
%%
%% Example:
%%
%%   {kernel,"ERTS  CXC 138 10","3.0.1"}
%%
%%-----------------------------------------------------------------------------
-type app_tpl() :: {
  Name :: atom(),
  Desc :: iolist(),
  Ver :: iolist()
}.

%%-----------------------------------------------------------------------------
%% A type definition defining a 'map representation' of an ERTS application.
%%
%% Example:
%%
%%   #{app_nfo => #{
%%     description => <<"ERTS  CXC 138 10">>,
%%     name => kernel,
%%     version => <<"3.0.1">>
%%     }
%%   }
%%
%%-----------------------------------------------------------------------------
-type app_nfo() :: #{
  app_nfo => #{
    name => Name :: binary(),
    description => Desc :: binary(),
    version => Ver :: binary()
  }
}.

%%-----------------------------------------------------------------------------
% A record definition defining a 'map representation of an ERTS application.
%%
%% Example:
%%
%%   #{app_rec{kernel,<<"ERTS  CXC 138 10">>,<<"3.0.1">>}}
%%
%%-----------------------------------------------------------------------------
-record(app_rec, {name :: atom(), description :: binary(), version :: binary()}).

%%-----------------------------------------------------------------------------
%% A type definition denoting the results of attempting to start an
%% OTP 'application'.
%%
%% Example:
%%
%%   {crypto,started}, {stdlib, already_running}, etc.
%%
%%-----------------------------------------------------------------------------
-type app_start_res() :: {
  App :: atom(),
  AppStarted :: started | already_running
}.

%%-----------------------------------------------------------------------------
%% A type definition denoting the results of attempting to start an
%% OTP 'application'.
%%
%% Example:
%%
%%   {crypto,stopped}, {stdlib, not_running}, etc.
%%
%%-----------------------------------------------------------------------------
-type app_stop_res() :: {
  App :: atom(),
  AppStopped :: stopped | not_running
}.


%%%============================================================================
%%% Public Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Return the 'currently loaded applications' in the system as list of
%% 'app_nfo' map entities.
%% @end
%%-----------------------------------------------------------------------------
-spec get_loaded_apps() -> LoadedApps :: [app_nfo()].

get_loaded_apps() ->
  lists:map(fun to_map/1, application:loaded_applications()).

%%-----------------------------------------------------------------------------
%% @doc
%% Return the 'currently running applications' in the system as list of
%% 'app_nfo' map entities.
%% @end
%%-----------------------------------------------------------------------------
-spec get_running_apps() -> RunningApps :: [app_nfo()].

get_running_apps() ->
  lists:map(fun to_map/1, application:which_applications()).

%%-----------------------------------------------------------------------------
%% @doc
%% Return the 'currently running applications' in the system as list of
%% 'app_nfo' map entities.
%% @end
%%-----------------------------------------------------------------------------
-spec get_app_cnfg(App :: atom()) -> AppCnfg :: map().

get_app_cnfg(App) ->
  {ok, KVs} = application:get_all_key(App),
  app_env_to_map(KVs).

%%-----------------------------------------------------------------------------
%% @doc
%% Start the specified OTP applications if they are not running.
%% @end
%%-----------------------------------------------------------------------------
-spec start_apps(Apps :: atom() | list(atom()))
      -> Res :: app_start_res() | list(app_start_res()).

start_apps(Apps) when is_list(Apps) ->
  [start_apps(App) || App <- Apps];
start_apps(App) when is_atom(App) ->
  case lists:keyfind(App, 1, application:which_applications()) of
    {App, _Desc, _Vsn} ->
      {App, app_running};
    false ->
      ok = application:start(App),
      {App, app_started}
  end.

%%-----------------------------------------------------------------------------
%% @doc
%% Stop the specified OTP applications if they are running.
%% @end
%%-----------------------------------------------------------------------------
-spec stop_apps(Apps :: atom() | list(atom()))
      -> Res :: app_stop_res() | list(app_stop_res()).

stop_apps(Apps) when is_list(Apps) ->
  [stop_apps(App) || App <- Apps];
stop_apps(App) when is_atom(App) ->
  case lists:keyfind(App, 1, application:which_applications()) of
    {App, _Desc, _Vsn} ->
      ok = application:stop(App),
      {App, app_stopped};
    false ->
      {App, app_not_running}
  end.

%%-----------------------------------------------------------------------------
%% @doc
%% Convert the specified 'app_tpl' 3-tuple entity to an 'app_nfo' map entity.
%%
%% ==== Example Input ====
%%   ```
%%   {kernel,"ERTS  CXC 138 10","3.0.1"}
%%   '''
%%
%% ==== Example Ouput ====
%%   ```
%%   #{app_nfo => #{
%%     description => <<"ERTS  CXC 138 10">>,
%%     name => <<"kernel">>,
%%     version => <<"3.0.1">>
%%     }
%%   }
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec to_map(From :: app_tpl()) -> To :: app_nfo().

to_map({Name, Desc, Ver}) ->
  io:format("Converting: ~p ~n", [{Name, Desc, Ver}]),
  AppMap = #{
    name => atom_to_binary(Name, utf8),
    description => list_to_binary(Desc),
    version => list_to_binary(Ver)
  },
  #{app_nfo => AppMap}.

%%-----------------------------------------------------------------------------
%% @doc
%% Convert the specified AppEnv entity into an expanded JSON Map.
%%
%% ==== Example Ouput ====
%%   ```
%%  {
%%    "applications": [
%%      "kernel", ...
%%    ],
%%    "description": "SASL  CXC 138 11",
%%    "env": {
%%      "errlog_type": "all", ...
%%      },
%%    "id": [],
%%    "included_applications": [],
%%    "maxP": "infinity",
%%    "maxT": "infinity",
%%    "mod": {
%%      "name": "sasl",
%%      "params": []
%%    },
%%    "modules": [
%%      "sasl", ...
%%    ],
%%    "registered": [
%%      "sasl_sup", ...
%%    ],
%%    "start_phases": "undefined",
%%    "vsn": "2.4.1"
%%  }
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec app_env_to_map([{Par :: atom(), Val :: term()}]) -> map().

app_env_to_map(AppEnv) ->
  io:format("app_env_to_map - AppEnv: ~p~n", [AppEnv]),
  Res = maps:from_list(AppEnv),
  Res1 = maps:update(description, list_to_binary(maps:get(description, Res)), Res),
  Res2 = maps:update(vsn, list_to_binary(maps:get(vsn, Res1)), Res1),
  {Name, Param} = maps:get(mod, Res2),
  Res3 = maps:update(mod, #{<<"name">> => Name, <<"params">> => Param}, Res2),
  Res4 = maps:update(env, maps:from_list((maps:get(env, Res3))), Res3),
  io:format("app_env_to_map - AppEnvMap: ~p~n", [Res4]),
  Res4.

%%-----------------------------------------------------------------------------
%% @deprecated
%% @doc
%% Convert the specified 'app_tpl' 3-tuple entity to an 'app_rec' record entity.
%%
%% ==== Example Input ====
%%   ```
%%   {kernel,"ERTS  CXC 138 10","3.0.1"}
%%   '''
%%
%% ==== Example Ouput ====
%%   ```
%%   app_rec#{kernel,<<"ERTS  CXC 138 10">>,<<"3.0.1">>}
%%   i.e:
%%   {app_rec, kernel, <<"ERTS  CXC 138 10">>, <<"3.0.1">>}
%%   }
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec to_record(From :: app_tpl()) -> To :: #app_rec{}.

to_record({Name, Desc, Ver}) ->
  #app_rec{
    % name = atom_to_binary(Name, utf8),
    name = Name,
    description = list_to_binary(Desc),
    version = list_to_binary(Ver)
  }.
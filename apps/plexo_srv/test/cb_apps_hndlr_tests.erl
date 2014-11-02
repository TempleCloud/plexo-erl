%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% Test the 'cb_apps_hndlr' module.
%%% @end
%%%----------------------------------------------------------------------------
-module(cb_apps_hndlr_tests).
-author("Temple").


-include_lib("eunit/include/eunit.hrl").

%%%============================================================================
%% Test Descriptions
%%%============================================================================

cb_apps_hndlr_test_() -> {
  "Test starting the applications required for the plexo_srv application.", [
    {setup, local,
      fun start_plexo_srv/0, fun stop_plexo_srv/1, fun test_start_app/1},
    {setup, local,
      fun start_plexo_srv/0, fun stop_plexo_srv/1, fun test_start_started_app/1},
    {setup, local,
      fun start_plexo_srv/0, fun stop_plexo_srv/1, fun test_get_running_apps/1},
    {setup, local,
      fun start_plexo_srv/0, fun stop_plexo_srv/1, fun test_get_loaded_apps/1}
  ]
}.

%%%============================================================================
%% Setup Functions
%%%============================================================================

start_plexo_srv() ->
  plexo_srv:start(),
  inets:start(),
  RestPath = <<"http://localhost:8877/api/apps">>,
  App = <<"sasl">>,
  {RestPath, App}.

stop_plexo_srv(_TestConfig) ->
  plexo_srv:stop(),
  inets:stop(),
  ok.

%%%============================================================================
%% Tests
%%%============================================================================

test_start_app({RestPath, App}) ->
  Res = start_remote_app(RestPath, App),
  #{<<"appName">> := AppName} = Res,
  #{<<"appStatus">> := AppStatus} = Res,
  ?_assertEqual(AppName, App),
  ?_assertEqual(AppStatus, <<"app_started">>).

test_start_started_app({RestPath, App}) ->
  start_remote_app(RestPath, App),
  Res = start_remote_app(RestPath, App),
  #{<<"appName">> := AppName} = Res,
  #{<<"appStatus">> := AppStatus} = Res,
  ?_assertEqual(AppName, App),
  ?_assertEqual(AppStatus, <<"app_running">>).

test_get_running_apps({RestPath, _App}) ->
  Res = get_remote_apps(RestPath, <<"?status=running">>),
  ?_assertEqual(Res, Res).

test_get_loaded_apps({RestPath, _App}) ->
  Res = get_remote_apps(RestPath, <<"?status=loaded">>),
  ?_assertEqual(Res, Res).

%%%============================================================================
%% Helper Methods
%%%============================================================================

start_remote_app(RestPath, App) ->

  Url = binary_to_list(RestPath) ++ "/" ++ binary_to_list(App),
  ContentType = "application/x-www-form-urlencoded",
  RqBody = <<"">>,

  {ok, {{_HttpVsn, 200, _StatusCode}, _Headers, RsBody}} =
    httpc:request(post, {Url, [], ContentType, RqBody}, [], []),

  Res = core_json:from_json(RsBody),
  ?debugFmt("Created App Resource: ~p~n", [Res]),
  Res.


get_remote_apps(RestPath, QueryParam) ->

  Url = binary_to_list(RestPath) ++ binary_to_list(QueryParam),

  {ok, {{_HttpVsn, 200, _StatusCode}, _Headers, RsBody}} =
    httpc:request(get, {Url, []}, [], []),

  Res = core_json:from_json(RsBody),
  ?debugFmt("Retrieved App Resource: ~p~n", [Res]),
  Res.

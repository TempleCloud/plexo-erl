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
      fun start_plexo_srv/0, fun stop_plexo_srv/1, fun test_get_loaded_apps/1},
    {setup, local,
      fun start_plexo_srv/0, fun stop_plexo_srv/1, fun test_get_apps_null_qp/1},
    {setup, local,
      fun start_plexo_srv/0, fun stop_plexo_srv/1, fun test_get_apps_bad_qp/1}
  ]
}.

%%%============================================================================
%% Setup Functions
%%%============================================================================

start_plexo_srv() ->
  plexo_srv:start(),
  inets:start(),
  User = "Temple",
  Passwd = "Wibble2Wobble",
  Uri = "http://localhost:8877/api/apps",
  App = sasl,
  Fixture = #{user => User, passwd => Passwd, uri => Uri, app => App},
  Fixture.

stop_plexo_srv(_Fixture) ->
  plexo_srv:stop(),
  inets:stop(),
  ok.

%%%============================================================================
%% Tests
%%%============================================================================

test_start_app(Fixture) ->
  Res = start_remote_app(Fixture),
  #{appName := AppName, appStatus := AppStatus} = Res,
  ?_assertEqual(maps:get(app, Fixture), AppName),
  ?_assertEqual(<<"app_started">>, AppStatus).

test_start_started_app(Fixture) ->
  start_remote_app(Fixture),
  Res = start_remote_app(Fixture),
  #{appName := AppName, appStatus := AppStatus} = Res,
  ?_assertEqual(maps:get(app, Fixture), AppName),
  ?_assertEqual(<<"app_running">>, AppStatus).

test_get_running_apps(Fixture) ->
  Res = get_remote_apps(Fixture, <<"?status=running">>),
  [?_assertEqual(true, is_valid_app_nfo(App)) || App <- Res].

test_get_loaded_apps(Fixture) ->
  Res = get_remote_apps(Fixture, <<"?status=loaded">>),
  [?_assertEqual(true, is_valid_app_nfo(App)) || App <- Res].

test_get_apps_null_qp(Fixture) ->
  Res = get_remote_apps(Fixture, <<"">>),
  ?_assertEqual({500, "Internal Server Error"}, Res).

test_get_apps_bad_qp(Fixture) ->
  Res = get_remote_apps(Fixture, <<"?status=bad">>),
  Expected = #{error => <<"Error. Bad QueryParam: 'bad'">>},
  ?_assertEqual(Expected, Res).


%%%============================================================================
%% Helper Methods
%%%============================================================================

start_remote_app(Fixture) ->

  #{user := User, passwd :=  Passwd, uri :=  Uri, app := App} = Fixture,

  Url = Uri ++ "/" ++ atom_to_list(App),
  ContentType = "application/x-www-form-urlencoded",
  RqHeaders = [util_inet:auth_header(User, Passwd), {"Content-Type",ContentType}],
  RqBody = <<"">>,
  RqOptions = [{body_format,binary}],

  {ok, {{_HttpVsn, 200, _StatusCode}, _Headers, RsBody}} =
    httpc:request(post, {Url, RqHeaders, ContentType, RqBody}, [], RqOptions),

  Res = core_json:from_json(RsBody),
  ?debugFmt("start_remote_app: ~p~n", [Res]),
  Res.


get_remote_apps(Fixture, QueryParam) ->

  #{user := User, passwd :=  Passwd, uri :=  Uri} = Fixture,

  Url = Uri ++ binary_to_list(QueryParam),
  RqHeaders = [util_inet:auth_header(User, Passwd)],
  RqHttpOptions = [],
  RqOptions = [],

  Res = case httpc:request(get, {Url, RqHeaders}, RqHttpOptions, RqOptions) of
    % Process valid HTTP 200 response. These should return JSON.
    {ok, {{_HttpVsn, 200, _StatusCode}, _Headers, RsBody}} ->
      core_json:from_json(RsBody);
    % Process valid HTTP 500 response. These should return JSON.
    {ok, {{_HttpVsn, 500, StatusCode}, _Headers, _RsBody}} ->
      {500, StatusCode};
    _ ->
      throw(unexpected_response)
    end,

  ?debugFmt("get_remote_apps: ~p~n", [Res]),
  Res.


is_valid_app_nfo(AppNfo) ->
  erts_apps_tests:is_valid_app_nfo(AppNfo).


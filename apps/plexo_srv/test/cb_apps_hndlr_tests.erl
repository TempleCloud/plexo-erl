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
  User = "Temple",
  Passwd = "Wibble2Wobble",
  Uri = <<"http://localhost:8877/api/apps">>,
  App = <<"sasl">>,
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
  #{<<"appName">> := AppName} = Res,
  #{<<"appStatus">> := AppStatus} = Res,
  ?_assertEqual(AppName, maps:get(app, Fixture)),
  ?_assertEqual(AppStatus, <<"app_started">>).

test_start_started_app(Fixture) ->
  start_remote_app(Fixture),
  Res = start_remote_app(Fixture),
  #{<<"appName">> := AppName} = Res,
  #{<<"appStatus">> := AppStatus} = Res,
  ?_assertEqual(AppName, maps:get(app, Fixture)),
  ?_assertEqual(AppStatus, <<"app_running">>).

test_get_running_apps(Fixture) ->
  Res = get_remote_apps(Fixture, <<"?status=running">>),
  ?_assertEqual(Res, Res).

test_get_loaded_apps(Fixture) ->
  Res = get_remote_apps(Fixture, <<"?status=loaded">>),
  ?_assertEqual(Res, Res).

%%%============================================================================
%% Helper Methods
%%%============================================================================

start_remote_app(Fixture) ->

  #{user := User, passwd :=  Passwd, uri :=  Uri, app := App} = Fixture,

  Url = binary_to_list(Uri) ++ "/" ++ binary_to_list(App),
  ContentType = "application/x-www-form-urlencoded",
  RqHeaders = [auth_header(User, Passwd), {"Content-Type",ContentType}],
  RqBody = <<"">>,
  RqOptions = [{body_format,binary}],

  {ok, {{_HttpVsn, 200, _StatusCode}, _Headers, RsBody}} =
    httpc:request(post, {Url, RqHeaders, ContentType, RqBody}, [], RqOptions),

  Res = core_json:from_json(RsBody),
  ?debugFmt("Created App Resource: ~p~n", [Res]),
  Res.


get_remote_apps(Fixture, QueryParam) ->

  #{user := User, passwd :=  Passwd, uri :=  Uri} = Fixture,

  Url = binary_to_list(Uri) ++ binary_to_list(QueryParam),
  RqHeaders = [auth_header(User, Passwd)],
  RqHttpOptions = [],
  RqOptions = [],

  {ok, {{_HttpVsn, 200, _StatusCode}, _Headers, RsBody}} =
    httpc:request(get, {Url, RqHeaders}, RqHttpOptions, RqOptions),

  Res = core_json:from_json(RsBody),
  ?debugFmt("Retrieved App Resource: ~p~n", [Res]),
  Res.


auth_header(User, Pass) ->
  Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
  {"Authorization","Basic " ++ Encoded}.
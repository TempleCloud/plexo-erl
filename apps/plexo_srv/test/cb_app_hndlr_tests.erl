%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% Test the 'cb_app_hndlr' module.
%%% @end
%%%----------------------------------------------------------------------------
-module(cb_app_hndlr_tests).
-author("Temple").

-include_lib("eunit/include/eunit.hrl").

-export([
  get_remote_app/1                % Create Http Basic Auth Header.
]).

%%%============================================================================
%% Test Descriptions
%%%============================================================================

cb_app_hndlr_test_() -> {
  "Test starting the applications required for the plexo_srv application.", [
  {setup, local,
    fun start_plexo_srv/0, fun stop_plexo_srv/1, fun test_start_app/1},
  {setup, local,
    fun start_plexo_srv/0, fun stop_plexo_srv/1, fun test_start_started_app/1},
  {setup, local,
    fun start_plexo_srv/0, fun stop_plexo_srv/1, fun test_stop_app/1},
  {setup, local,
    fun start_plexo_srv/0, fun stop_plexo_srv/1, fun test_stop_stopped_app/1},
  {setup, local,
    fun start_plexo_srv/0, fun stop_plexo_srv/1, fun test_get_app/1}
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
  Uri = "http://localhost:8877/api/app",
  App = sasl,
  Fixture = #{user => User, passwd => Passwd, uri => Uri, app => App},
  Fixture.

stop_plexo_srv(Fixture) ->
  stop_remote_app(Fixture),
  plexo_srv:stop(),
  inets:stop(),
  ok.

%%%============================================================================
%% Tests
%%%============================================================================

test_start_app(Fixture) ->
  stop_remote_app(Fixture),
  Res = start_remote_app(Fixture),
  #{appName := AppName} = Res,
  #{appStatus := AppStatus} = Res,
  % ?_assertEqual(AppName, maps:get(app, Fixture)),
  ?assertEqual(atom_to_binary(maps:get(app, Fixture), utf8), AppName),
  ?assertEqual(AppStatus, <<"app_started">>),
  ?_assertEqual(ok, ok).

test_start_started_app(Fixture) ->
  start_remote_app(Fixture),
  Res = start_remote_app(Fixture),
  #{appName := AppName} = Res,
  #{appStatus := AppStatus} = Res,
  % ?_assertEqual(AppName, maps:get(app, Fixture)),
  ?assertEqual(atom_to_binary(maps:get(app, Fixture), utf8), AppName),
  ?assertEqual(AppStatus, <<"app_running">>),
  ?_assertEqual(ok, ok).

test_stop_app(Fixture) ->
  start_remote_app(Fixture),
  Res = stop_remote_app(Fixture),
  #{appName := AppName} = Res,
  #{appStatus := AppStatus} = Res,
  % ?_assertEqual(AppName, maps:get(app, Fixture)),
  ?assertEqual(atom_to_binary(maps:get(app, Fixture), utf8), AppName),
  ?assertEqual(AppStatus, <<"app_stopped">>),
  ?_assertEqual(ok, ok).

test_stop_stopped_app(Fixture) ->
  stop_remote_app(Fixture),
  Res = stop_remote_app(Fixture),
  #{appName := AppName} = Res,
  #{appStatus := AppStatus} = Res,
  % ?_assertEqual(AppName, maps:get(app, Fixture)),
  ?assertEqual(atom_to_binary(maps:get(app, Fixture), utf8), AppName),
  ?assertEqual(AppStatus, <<"app_not_running">>),
  ?_assertEqual(ok, ok).

test_get_app(Fixture) ->
  start_remote_app(Fixture),
  Res = get_remote_app(Fixture),
  #{
    applications := _Applications,
    description := _Description,
    env := _Env,
    id := _Id,
    included_applications := _IncludedApplications,
    maxP := _MaxP,
    maxT := _MaxT,
    mod := Mod,
    modules := _Modules,
    registered := _Registered,
    start_phases := _StartPhase,
    vsn := _VSN
  } = Res,
  [{ModName, _ModCnfg}] = maps:to_list(Mod),
  ?_assertEqual(maps:get(app, Fixture), ModName).


%%%============================================================================
%% Helper Methods
%%%============================================================================


start_remote_app(Fixture) ->

  #{user := User, passwd :=  Passwd, uri :=  Uri, app := App} = Fixture,

  Url = Uri ++ "/" ++ atom_to_list(App),
  ContentType = "application/x-www-form-urlencoded",
  RqHeaders = [util_inet:auth_header(User, Passwd), {"Content-Type",ContentType}],
  RqOptions = [{body_format,binary}],
  RqBody = <<"">>,

  {ok, {{_HttpVsn, 200, _StatusCode}, _Headers, RsBody}} =
    httpc:request(put, {Url, RqHeaders, ContentType, RqBody}, [], RqOptions),

  Res = core_json:from_json(RsBody),
  ?debugFmt("Created App Resource (Authed): ~p~n", [Res]),
  Res.


stop_remote_app(Fixture) ->

  #{user := User, passwd :=  Passwd, uri :=  Uri, app := App} = Fixture,

  Url = Uri ++ "/" ++ atom_to_list(App),
  RqHeaders = [util_inet:auth_header(User, Passwd)],
  RqHttpOptions = [],
  RqOptions = [],

  {ok, {{_HttpVsn, 200, _StatusCode}, _RsHeaders, RsBody}} =
    httpc:request(delete, {Url, RqHeaders}, RqHttpOptions, RqOptions),

  Res = core_json:from_json(RsBody),
  ?debugFmt("Deleted App Resource: ~p~n", [Res]),
  Res.


get_remote_app(Fixture) ->

  #{user := User, passwd :=  Passwd, uri :=  Uri, app := App} = Fixture,

  Url = Uri ++ "/" ++ atom_to_list(App),
  RqHeaders = [util_inet:auth_header(User, Passwd)],
  RqHttpOptions = [],
  RqOptions = [],

  Res = case httpc:request(get, {Url, RqHeaders}, RqHttpOptions, RqOptions) of
    % Process valid HTTP 200 response. These should return JSON.
    {ok, {{_HttpVsn, 200, _StatusCode}, _Headers, RsBody}} ->
      core_json:from_json(RsBody);
    % Process valid HTTP 401 response. These should return JSON.
    {ok, {{_HttpVsn, 401, StatusCode}, Headers, _RsBody}} ->
      {_, Realm} = lists:keyfind("www-authenticate", 1, Headers),
      {401, StatusCode, Realm};
    _ ->
      throw(unexpected_response)
    end,

  ?debugFmt("Retrieved App Resource: ~p~n", [Res]),
  Res.


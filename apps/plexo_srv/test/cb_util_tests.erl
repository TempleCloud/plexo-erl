%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% Test the 'cb_util' module.
%%% @end
%%%----------------------------------------------------------------------------
-module(cb_util_tests).
-author("Temple").



-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%============================================================================
%% Test Descriptions
%%%============================================================================

cb_util_test_() -> {
  "Test starting the applications required for the plexo_srv application.", [
    {setup, local,
      fun start_plexo_srv/0, fun stop_plexo_srv/1,
      fun itest_valid_basic_auth/1
      },
    {setup, local,
      fun start_plexo_srv/0, fun stop_plexo_srv/1,
      fun itest_invalid_basic_auth/1
      }
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
  Uri = <<"http://localhost:8877/api/app">>,
  App = <<"kernel">>,
  Fixture = #{user => User, passwd => Passwd, uri => Uri, app => App},
  Fixture.

stop_plexo_srv(_Fixture) ->
  plexo_srv:stop(),
  inets:stop(),
  ok.

%%%============================================================================
%% Tests
%%%============================================================================

% Use the simple 'get_remote_app' rest function to test the login.
itest_valid_basic_auth(Fixture) ->
  Res = cb_app_hndlr_tests:get_remote_app(Fixture),
  #{
    <<"mod">> := #{<<"name">> := ModName, <<"params">> := _ModParams}
  } = Res,
  ?_assertEqual(ModName, maps:get(app, Fixture)).

itest_invalid_basic_auth(Fixture) ->
  InvalidFixture = maps:update(passwd, "BadPassword", Fixture),
  Res = cb_app_hndlr_tests:get_remote_app(InvalidFixture),
  ?_assertEqual({401, "Unauthorized", "Basic realm=\"plexo\""}, Res).


%% utest_build_peer() ->
%%
%%   % peer = undefined :: undefined | {inet:ip_address(), inet:port_number()},
%%   #cowboy_rq:http_req{peer={{127,0,0,1}, 51591}}.
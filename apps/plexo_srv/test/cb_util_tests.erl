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

%%%============================================================================
%% Unit Test Descriptions
%%%============================================================================

cb_util_unt_test_() -> {
  "Test starting the applications required for the plexo_srv application.", [
    {setup, local,
      fun su_unit_test/0, fun td_unit_test/1, fun utest_build_auth/1},
    {setup, local,
      fun su_unit_test/0, fun td_unit_test/1, fun utest_build_peer/1}
  ]
}.

%%%============================================================================
%% Unit Test Setup Functions
%%%============================================================================

su_unit_test() ->
  ok.

td_unit_test(_Fixture) ->
  ok.

%%%============================================================================
%% Unit Tests
%%%============================================================================

utest_build_auth(_Fixture) ->

  meck:new(cowboy_req, [non_strict]),
  meck:expect(cowboy_req, parse_header, fun(<<"authorization">>, MockRes) -> MockRes end),

  Res = cb_util:build_auth({bad_input}),
  ?_assertEqual(undefined, Res),

  ?_assertEqual(
    #{user => <<"Temple">>, passwd => <<"Wibble2Wobble">>, type => <<"basic">>},
    cb_util:build_auth({<<"basic">>, {<<"Temple">>, <<"Wibble2Wobble">>}})
  ),

  meck:validate(cowboy_req),
  meck:unload(cowboy_req),

  ?_assertEqual(ok, ok).

utest_build_peer(_Fixture) ->

  meck:new(cowboy_req, [non_strict]),
  meck:expect(cowboy_req, peer, fun(MockRes) -> MockRes end),

  Res = cb_util:build_peer({bad_input}),
  ?_assertEqual(undefined, Res),

  ?_assertEqual(
    #{ip => {127,0,0,1}, port => 1234},
    cb_util:build_peer({{127,0,0,1}, 1234})
  ),

  meck:validate(cowboy_req),
  meck:unload(cowboy_req),

  ?_assertEqual(ok, ok).

%%%============================================================================
%% Integration Test Runner
%%%============================================================================

cb_util_itg_test_() -> {
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
%% Integration Test Setup/Fixture Functions
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
%% Integration Tests
%%%============================================================================

% Use the simple 'get_remote_app' rest function to test the login.
itest_valid_basic_auth(Fixture) ->
  Res = cb_app_hndlr_tests:get_remote_app(Fixture),
  #{
    mod := #{name := ModName, params := _ModParams}
  } = Res,
  ?_assertEqual(ModName, maps:get(app, Fixture)).

itest_invalid_basic_auth(Fixture) ->
  InvalidFixture = maps:update(passwd, "BadPassword", Fixture),
  Res = cb_app_hndlr_tests:get_remote_app(InvalidFixture),
  ?_assertEqual({401, "Unauthorized", "Basic realm=\"plexo\""}, Res).



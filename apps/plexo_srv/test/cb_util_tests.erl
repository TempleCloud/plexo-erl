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
%% Test Descriptions
%%%============================================================================

cb_apps_hndlr_test_() -> {
  "Test starting the applications required for the plexo_srv application.", [
    {setup, local,
      fun start_plexo_srv/0, fun stop_plexo_srv/1, fun test_valid_basic_auth/1},
    {setup, local,
      fun start_plexo_srv/0, fun stop_plexo_srv/1, fun test_invalid_basic_auth/1}
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
test_valid_basic_auth(Fixture) ->
  Res = get_remote_app(Fixture),
  #{
    <<"mod">> := #{<<"name">> := ModName, <<"params">> := _ModParams}
  } = Res,
  ?_assertEqual(ModName, maps:get(app, Fixture)).

test_invalid_basic_auth(Fixture) ->
  InvalidFixture = maps:update(passwd, "BadPassword", Fixture),
  Res = get_remote_app(InvalidFixture),
  ?_assertEqual({401, "Unauthorized", "Basic realm=\"plexo\""}, Res).


%%%============================================================================
%% Helper Methods
%%%============================================================================

get_remote_app(Fixture) ->

  #{user := User, passwd :=  Passwd, uri :=  Uri, app := App} = Fixture,

  Url = binary_to_list(Uri) ++ "/" ++ binary_to_list(App),
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
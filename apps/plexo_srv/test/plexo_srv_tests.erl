%%%-------------------------------------------------------------------
%%% Created : 17. Sep 2014 19:43
%%%
%%% @author Temple
%%%
%%% @doc
%%% A helper class to start the 'plexo_srv' server application.
%%% @end
%%%-------------------------------------------------------------------
-module(plexo_srv_tests).
-author("Temple").

-include_lib("eunit/include/eunit.hrl").

%%%============================================================================
%% Public Function Tests
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------


%%%============================================================================
%% Test Descriptions
%%%============================================================================

start_server_test_() -> {
  "Test starting the applications required for the plexo_srv application.",
    {setup, local,
      fun start_plexo_srv/0, fun stop_plexo_src/1, fun check_started_apps/1}
  }.


%%%============================================================================
%% Setup Functions
%%%============================================================================

start_plexo_srv() ->
  Res = plexo_srv:start(),
  ?debugFmt("Started Apps: ~p~n", [Res]),
  Res.

stop_plexo_src(Apps) ->
  Res = plexo_srv:stop(),
  ?debugFmt("Stopped Apps: ~p~n", [Res]),
  Res.


%%%============================================================================
%% Tests
%%%============================================================================

check_started_apps(Apps) ->
  ?_assertEqual(ok, ok).


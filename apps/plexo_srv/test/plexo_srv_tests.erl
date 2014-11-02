%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% Test the 'plexo_srv' module.
%%% @end
%%%----------------------------------------------------------------------------
-module(plexo_srv_tests).
-author("Temple").

-include_lib("eunit/include/eunit.hrl").

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
  StartedApps = plexo_srv:start(),
  ?debugFmt("Started Apps: ~p~n", [StartedApps]),
  StartedApps.

stop_plexo_src(_StartedApps) ->
  StoppedApps = plexo_srv:stop(),
  ?debugFmt("Stopped Apps: ~p~n", [StoppedApps]),
  StoppedApps.

%%%============================================================================
%% Tests
%%%============================================================================

check_started_apps(_StartedApps) ->
  ?_assertEqual(ok, ok).


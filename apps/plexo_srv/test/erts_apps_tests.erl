%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% Test the 'erts_apps' module.
%%% @end
%%%----------------------------------------------------------------------------
-module(erts_apps_tests).
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
get_running_apps_test_() ->
  [test_get_running_apps()].

test_get_running_apps() ->

  Expected = mock_running_apps(),
  ?debugFmt("Expected Running Apps: ~p~n", [Expected]),

  % Actual = erts_apps:get_loaded_apps(),
  Actual = erts_apps:get_running_apps(),
  ?debugFmt("Actual Running Apps: ~p~n", [Actual]),

  ?_assertEqual(Expected, Actual).

%%-----------------------------------------------------------------------------
%% @todo Soften this strict test to make it robust to version chnages etc.
mock_running_apps() ->
  [
    #{app_nfo => #{
      description => <<"ERTS  CXC 138 10">>,
      name => <<"stdlib">>,
      version => <<"2.2">>}
    },
    #{app_nfo => #{
      description => <<"ERTS  CXC 138 10">>,
      name => <<"kernel">>,
      version => <<"3.0.3">>
      }
    }
  ].


%%-----------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
to_map_test_() ->
  [test_to_map()].

test_to_map() ->
  Expected = mock_test_app_nfo(),
  Input = mock_test_app_tpl(),
  Result = erts_apps:to_map(Input),
  ?_assertEqual(Expected, Result).


mock_test_app_tpl() ->
  {test_app_name, "A test app!", "0.0.0"}.

mock_test_app_nfo() ->
  #{app_nfo => #{
    description => <<"A test app!">>,
    name => <<"test_app_name">>,
    version => <<"0.0.0">>
    }
  }.



%%-----------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
to_record_test_() ->
  [test_to_record()].

test_to_record() ->
  Expected = mock_test_app_rec(),
  Input = mock_test_app_tpl(),
  Result = erts_apps:to_record(Input),
  ?_assertEqual(Expected, Result).

mock_test_app_rec() ->
  %%   #app_rec{
  %%     name = test_app_name,
  %%     description = <<"A test app!">>,
  %%     version =  <<"0.0.0">>
  %%   }.
  {app_rec, test_app_name, <<"A test app!">>,<<"0.0.0">>}.


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
%% Export Test Util Function
%%%============================================================================

-export([
  is_valid_app_nfo/1                   % Start the plexo_srv app.
]).

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
  Actual = erts_apps:get_running_apps(),
  [?_assertEqual(true, is_valid(App)) || App <- Actual].

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
  {app_rec, test_app_name, <<"A test app!">>,<<"0.0.0">>}.


is_valid_app_nfo(AppNfo) ->
  #{
    app_nfo := #{
      description := Desc, name := Name, version := Vsn
    }
  } = AppNfo,
  case {is_binary(Desc), is_binary(Name), is_binary(Vsn)} of
    {true, true, true} -> true;
    _                  -> false
  end.
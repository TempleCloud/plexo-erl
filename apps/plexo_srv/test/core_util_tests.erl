%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% Test the 'core_util' module.
%%% @end
%%%----------------------------------------------------------------------------
-module(core_util_tests).
-author("Temple").

-include_lib("eunit/include/eunit.hrl").


%%%============================================================================
%%% Function Tests
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Test the 'to_atom_key_map' function.
%% @end
%%-----------------------------------------------------------------------------
to_atom_key_map_test_() -> [
  test_single_map(),
  test_single_nested_map(),
  test_no_map_list(),
  test_integer_list(),
  test_single_map_list(),
  test_single_nested_map_list(),
  test_multi_map_list(),
  test_multi_nested_map_list()
  ].

test_no_map_list() ->
  Input =  [],
  Expected = [],
  Result = core_util:to_atom_key_map(Input),
  ?_assertEqual(Expected, Result).

test_integer_list() ->
  Input =  1,
  Expected = 1,
  Result = core_util:to_atom_key_map(Input),
  ?_assertEqual(Expected, Result).

test_single_map() ->
  Input =  mock_app_data_map(),
  Expected = mock_ak_app_data_map(),
  Result = core_util:to_atom_key_map(Input),
  ?_assertEqual(Expected, Result).

test_single_nested_map() ->
  Input =  mock_app_nfo(),
  Expected = mock_ak_app_nfo(),
  Result = core_util:to_atom_key_map(Input),
  ?_assertEqual(Expected, Result).

test_single_map_list() ->
  Input =  [mock_app_data_map()],
  Expected = [mock_ak_app_data_map()],
  Result = core_util:to_atom_key_map(Input),
  ?_assertEqual(Expected, Result).

test_single_nested_map_list() ->
  Input =  [mock_app_nfo()],
  Expected = [mock_ak_app_nfo()],
  Result = core_util:to_atom_key_map(Input),
  ?_assertEqual(Expected, Result).

test_multi_map_list() ->
  Input =  [mock_app_data_map() || _ <- lists:seq(1, 3)],
  Expected = [mock_ak_app_data_map() || _ <- lists:seq(1, 3)],
  Result = core_util:to_atom_key_map(Input),
  ?_assertEqual(Expected, Result).

test_multi_nested_map_list() ->
  Input =  [mock_app_nfo() || _ <- lists:seq(1, 3)],
  Expected = [mock_ak_app_nfo() || _ <- lists:seq(1, 3)],
  Result = core_util:to_atom_key_map(Input),
  ?_assertEqual(Expected, Result).


%%%============================================================================
%% Mocks
%%%============================================================================

mock_ak_app_nfo() ->
  #{app_nfo => mock_ak_app_data_map()}.

mock_ak_app_data_map() ->
  #{
    description => <<"ERTS  CXC 138 10">>,
    name => <<"kernel">>,
    version => <<"3.0.3">>
  }.


mock_app_nfo() ->
  #{<<"app_nfo">> => mock_app_data_map()}.

mock_app_data_map() ->
  #{
    <<"description">> => <<"ERTS  CXC 138 10">>,
    <<"name">> => <<"kernel">>,
    <<"version">> => <<"3.0.3">>
  }.


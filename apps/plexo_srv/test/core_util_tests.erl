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
%%% Test - to_atom_keymap/0
%%%============================================================================

to_atom_keymap_test_() -> [
  test_single_map(),
  test_single_nested_map(),
  test_no_map_list(),
  test_integer_list(),
  test_single_map_list(),
  test_single_nested_map_list(),
  test_multi_map_list(),
  test_multi_nested_map_list()
  ].

%%% Tests ---------------------------------------------------------------------

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

%%% Mocks ---------------------------------------------------------------------

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

%%%============================================================================
%%% Test - proplist_to_map/0
%%%============================================================================

proplist_to_map_test_() -> [
  test_proplist_to_map(),
  test_charlist_proplist_to_map1(),
  test_charlist_proplist_to_map2(),
  test_vallist_proplist_to_map1(),
  test_vallist_proplist_to_map2()
].

test_proplist_to_map() ->
  Input =  [{k1, v1}, {k2, v2}, {k3, v3}],
  Expected = #{k1 => v1, k2 => v2, k3 => v3},
  Result = core_util:proplist_to_map(Input),
  ?_assertEqual(Expected, Result).

test_charlist_proplist_to_map1() ->
  Input =  [{k1, v1}, {k2, [$h,$e,$l,$l,$o]}, {k3, v3}],
  Expected = #{k1 => v1, k2 => <<"hello">>, k3 => v3},
  Result = core_util:proplist_to_map(Input),
  ?_assertEqual(Expected, Result).

test_charlist_proplist_to_map2() ->
  Input =  [{k1, v1}, {k2, [104,101,108,108,111]}, {k3, v3}],
  Expected = #{k1 => v1, k2 => <<"hello">>, k3 => v3},
  Result = core_util:proplist_to_map(Input),
  ?_assertEqual(Expected, Result).

test_vallist_proplist_to_map1() ->
  Input =  [{k1, v1}, {k2, ["1","2","3"]}, {k3, v3}],
  Expected = #{k1 => v1, k2 => ["1","2","3"], k3 => v3},
  Result = core_util:proplist_to_map(Input),
  ?_assertEqual(Expected, Result).

test_vallist_proplist_to_map2() ->
  Input =  [{k1, v1}, {k2, [one,two,three]}, {k3, v3}],
  Expected = #{k1 => v1, k2 => [one,two,three], k3 => v3},
  Result = core_util:proplist_to_map(Input),
  ?_assertEqual(Expected, Result).


%%%============================================================================
%%% Test - is_proplist/0
%%%============================================================================

is_proplist_test_() -> [
  test_is_proplist_single(),
  test_is_proplist_flat(),
  test_is_proplist_nested_1(),
  test_is_proplist_nested_2(),
  test_is_proplist_deep_nested_1(),
  test_is_proplist_deep_nested_2()
].

not_proplist_test_() -> [
  test_not_proplist_single(),
  test_not_proplist_flat()
].

%%% Tests ---------------------------------------------------------------------

test_is_proplist_single() ->
  Input =  [{k1, v1}],
  Expected = true,
  Result = core_util:is_proplist(Input),
  ?_assertEqual(Expected, Result).

test_is_proplist_flat() ->
  Input =  [{k1, v1}, {k2, v2}, {k3, v3}],
  Expected = true,
  Result = core_util:is_proplist(Input),
  ?_assertEqual(Expected, Result).

test_is_proplist_nested_1() ->
  Input =  [{k1, v1}, {k2, {k21, v21}}, {k3, v3}],
  Expected = true,
  Result = core_util:is_proplist(Input),
  ?_assertEqual(Expected, Result).

test_is_proplist_nested_2() ->
  Input =  [{k1, v1}, {k2, [non_prop, {k22, v22}]}, {k3, v3}],
  Expected = true, % is_proplist is/1 currently a shallow test.
  Result = core_util:is_proplist(Input),
  ?_assertEqual(Expected, Result).

test_is_proplist_deep_nested_1() ->
  Input =  mock_deep_nested_proplist(),
  Expected = true,
  Result = core_util:is_proplist(Input),
  ?_assertEqual(Expected, Result).

test_is_proplist_deep_nested_2() ->
  Input =  mock_deep_nested_non_proplist(),
  Expected = true, % is_proplist is/1 currently a shallow test.
  Result = core_util:is_proplist(Input),
  ?_assertEqual(Expected, Result).

test_not_proplist_single() ->
  Input =  {k1, v1},
  Expected = false,
  Result = core_util:is_proplist(Input),
  ?_assertEqual(Expected, Result).

test_not_proplist_flat() ->
  Input =  [{k1, v1}, non_prop, {k3, v3}],
  Expected = false,
  Result = core_util:is_proplist(Input),
  ?_assertEqual(Expected, Result).


%%% Mocks  --------------------------------------------------------------------

mock_deep_nested_proplist() ->
  [
    {k1, v1},
    {k2, [
      {k21, v21},
      {k22, {k221, v221}},
      {k23, [{k231, v231}, {k232, v232}]}
    ]},
    {k3, v3}
  ].

mock_deep_nested_non_proplist() ->
  [
    {k1, v1},
    {k2, [
      {k21, v21},
      {k22, {k221, v221}},
      {k23, [{k231, v231}, non_prop]}
    ]},
    {k3, v3}
  ].

%%%============================================================================
%%% Test - is_proplist/0
%%%============================================================================

is_property_test_() -> [
  test_is_property(),
  test_non_property()
].

%%% Tests ---------------------------------------------------------------------

test_is_property() ->
  Input =  mock_property(),
  Expected = true,
  Result = core_util:is_property(Input),
  ?_assertEqual(Expected, Result).

test_non_property() ->
  Input =  mock_non_property(),
  Expected = false,
  Result = core_util:is_property(Input),
  ?_assertEqual(Expected, Result).

%%% Mocks  --------------------------------------------------------------------

mock_property() ->
  {k1, v1}.

mock_non_property() ->
  non_property.
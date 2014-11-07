%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% Test the 'core_json' module.
%%% @end
%%%----------------------------------------------------------------------------
-module(core_json_tests).
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
to_json_test_() ->
  [test_to_json()].

test_to_json() ->
  Expected = mock_app_nfo_json_bin_str(),
  Result = core_json:to_json(mock_app_nfo()),
  ?_assertEqual(Expected, Result).


%%-----------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
from_json_test_() ->
  [test_from_bin_json(), test_from_charlist_json()].

test_from_bin_json() ->
  Expected = mock_app_nfo(),
  Result = core_json:from_json(mock_app_nfo_json_bin_str()),
  ?_assertEqual(Expected, Result).

test_from_charlist_json() ->
  Expected = mock_app_nfo(),
  Result = core_json:from_json(mock_app_nfo_json_charlist_str()),
  ?_assertEqual(Expected, Result).

%%%============================================================================
%% Mocks
%%%============================================================================

mock_app_nfo() ->
  #{app_nfo => mock_app_data_map()}.

mock_app_data_map() ->
  #{
    description => <<"ERTS  CXC 138 10">>,
    name => <<"kernel">>,
    version => <<"3.0.1">>
  }.

%% mock_app_nfo() ->
%%   #{<<"app_nfo">> => mock_app_data_map()}.
%%
%% mock_app_data_map() ->
%%   #{
%%     <<"description">> => <<"ERTS  CXC 138 10">>,
%%     <<"name">> => <<"kernel">>,
%%     <<"version">> => <<"3.0.1">>
%%   }.

%%-----------------------------------------------------------------------------
%% @todo  Make multiline...
mock_app_nfo_json_bin_str() ->
  <<"{\"app_nfo\":{\"description\":\"ERTS  CXC 138 10\",\"name\":\"kernel\",\"version\":\"3.0.1\"}}">>.

mock_app_nfo_json_charlist_str() ->
  "{\"app_nfo\":{\"description\":\"ERTS  CXC 138 10\",\"name\":\"kernel\",\"version\":\"3.0.1\"}}".

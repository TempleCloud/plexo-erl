%%%-------------------------------------------------------------------
%%% @author Temple
%%% @copyright (C) 2014, Lateral Industries
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2014 18:21
%%%-------------------------------------------------------------------
-module(core_auth_tests).
-author("Temple").

-include_lib("eunit/include/eunit.hrl").

%%%============================================================================
%% Unit Tests
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Unit tests for 'restful_auth' function.
%% @end
%%-----------------------------------------------------------------------------
restful_auth_test_() -> [
  test_restful_auth_undefined()
].

test_restful_auth_undefined() ->
  Input = #{auth => undefined},
  Output = core_auth:restful_auth(Input),
  ?_assertEqual(false, Output).


%%-----------------------------------------------------------------------------
%% @doc
%% Unit tests for 'do_auth' function.
%% @end
%%-----------------------------------------------------------------------------
do_auth_test_() -> [
  test_do_auth_undefined()
].

test_do_auth_undefined() ->
  Input = {undefined, undefined, undefined},
  Output = core_auth:do_auth(Input),
  ?_assertEqual(false, Output).
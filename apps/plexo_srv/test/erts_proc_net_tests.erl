%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% Test the 'erts_apps' module.
%%% @end
%%%----------------------------------------------------------------------------
-module(erts_proc_net_tests).
-author("Temple").

-include_lib("eunit/include/eunit.hrl").

%%%============================================================================
%% Export Test Util Function
%%%============================================================================

%% -export([
%%   is_valid_app_nfo/1                   % Validate an 'app_nfo' map.
%% ]).

%%%============================================================================
%% Public Function Tests
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Test that the {@link erts_apps:get_running_apps/0} function returns a list
%% of 'app_nfo" maps entities.
%% @end
%%-----------------------------------------------------------------------------
add_node_test_() ->
  [test_add_node1(), test_add_node2()].

test_add_node1() ->

  {A, B, C} = erts_proc_net:get_otp_app_root_pids(kernel),
  ANode = erts_proc_net:proc_vtx_nfo(A),
  BNode = erts_proc_net:proc_vtx_nfo(B, A),
  CNode = erts_proc_net:proc_vtx_nfo(C, B),

  {new, ANode2} = erts_proc_net:add_node(ANode, BNode),
  {new, ANode3} = erts_proc_net:add_node(ANode2, CNode),

  ?_assertEqual(ok, ok).

test_add_node2() ->

  {A, B, C} = erts_proc_net:get_otp_app_root_pids(kernel),
  ANode = erts_proc_net:proc_vtx_nfo(A),
  BNode = erts_proc_net:proc_vtx_nfo(B, A),
  CNode = erts_proc_net:proc_vtx_nfo(C, B),

  {new, ANode2} = erts_proc_net:add_node(ANode, BNode),
  {new, ANode3} = erts_proc_net:add_node(ANode2, CNode),

  CNChs = erts_proc_net:get_otp_pnode_children(CNode),
  % Res = [erts_proc_net:add_node(ANode3, CNChild) || CNChild <- CNChildren],


  ?_assertEqual(ok, ok).
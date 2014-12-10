%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% The {@module} module provides functions for determining the known
%%% structured relatinships between processes in an erlang runtime.
%%% @end
%%%----------------------------------------------------------------------------
-module(erts_proc_net).
-author("Temple").

%%% Export all declared functions when TEST.
-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
  -compile(export_all).
-endif.

%%%============================================================================
%%% Public API
%%%============================================================================

-export([
  get_otp_app_root_pids/1,
  get_otp_pid_children/1
  % get_otp_app_root_tmap/1
]).

-export([
  proc_vtx_nfo/1,
  proc_vtx_nfo/2
]).

-export([
  build_reg_proc_graph/0,
  build_reg_proc_graph/1,
  build_proc_graph/1
]).

-export([
  get_otp_app_root_tmap/1,
  get_otp_pnode_children/1,
  % find_node/2,
  add_node/2,
  add_child/2
  % display/1
]).


%%%============================================================================
%%% Public Types
%%%============================================================================

%%%============================================================================
%%% Public Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Return the first three root Pids of the specified OTP applicaton.
%%
%% This method assumes the application has been setup and initialised correctly
%% w.r.t the Erlang OTP conventions.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_otp_app_root_pids(AppName :: atom())
      -> Root :: {pid(), pid(), pid()}.

get_otp_app_root_pids(AppName) ->
  % Determine the name of the applications OTP supervisor by convention.
  % This should always exist as a named process...
  % TODO: Probably can get this from the app config...
  AppSupName = list_to_atom(atom_to_list(AppName) ++ "_sup"),

  % Get the process info associated with the supervisor...
  #{
    pid := AppSupPid,
    group_leader := GL,
    pd_ancestors := ANs
  } = erts_proc_net:proc_vtx_nfo(whereis(AppSupName)),

  % Assume there is only one parent, and, it is declared as an ancestor in the
  % process dictionary.
  AppSupMasterPid = hd(ANs),

  % Assume the AppMaster is always defined as the GroupLeader.
  AppMasterPid = GL,

  {AppMasterPid, AppSupMasterPid, AppSupPid}.


get_otp_app_root_tmap(AppName) ->
  {Root, Master, Sup} = get_otp_app_root_pids(AppName),

  RootNode = proc_vtx_nfo(Root),
  MasterNode = proc_vtx_nfo(Master, Root),
  SupNode = proc_vtx_nfo(Sup, Master),

  {new, RootNode2} = add_node(RootNode, MasterNode),
  {new, RootNode3} = add_node(RootNode2, SupNode),
  RootNode3.



get_otp_pid_children(Pid) ->
  #{links := Links} = proc_vtx_nfo(Pid),
  [LPid || LPid <- Links, LPid /= Pid].

get_otp_pnode_children(PNode) ->
  #{pid := PPid, links := Links} = PNode,
  [proc_vtx_nfo(LPid, PPid) || LPid <- Links, LPid /= PPid].

%% get_otp_app_root_tmap(AppName) ->
%% %%   {AppMasterPid, AppSupMasterPid, AppSupPid} = get_otp_app_root_pids(AppName),
%% %%   X1 = proc_vtx_nfo(AppSupPid),
%% %%   X2 = maps:put(children, [X1], proc_vtx_nfo(AppSupMasterPid)),
%% %%   maps:put(children, [X2], proc_vtx_nfo(AppMasterPid)).
%%   {AppMasterPid, AppSupMasterPid, AppSupPid} = get_otp_app_root_pids(AppName),
%%   RN = proc_vtx_nfo(AppMasterPid),
%%   {RN1, CN1} = add_child(AppSupMasterPid, RN),
%%   {RN2, _CN2} = add_child(AppSupPid, CN1),
%%   {RN, RN1, RN2}.

%% add_child(CPid, PNode) ->
%%   #{pid := PPid, children := PChildren} = PNode,
%%   CNode = proc_vtx_nfo(CPid),
%%   CNode1 = maps:put(parents, [PPid], CNode),
%%   PNode1 = maps:put(children, [CNode1 | PChildren], PNode),
%%   {PNode1, CNode1}.
%%   case Children of
%%     undefinded            -> maps:put(children, [CNode], Node);
%%     CNs when is_list(CNs) -> [CNode | CNs], CNode
%%   end,
%%   CNode.


%% get_otp_app_root_tmap(AppName) ->
%%   {AppMasterPid, AppSupMasterPid, AppSupPid} = get_otp_app_root_pids(AppName),
%%   RN = proc_vtx_nfo(AppMasterPid),
%%   CN1 = proc_vtx_nfo(AppSupMasterPid),
%%   CN2 = proc_vtx_nfo(AppSupPid),
%%
%%   RN1 = add_node(RN, CN1, AppMasterPid),
%%   RN2 = add_node(RN1, CN2, AppSupMasterPid),
%%   RN2.

%% -spec add_node(map(), map() | list(), pid()) -> map().
%% add_node(Nodes, ToAdd, Pid) when is_list(Nodes) ->
%%   [add_node(Node, ToAdd, Pid) || Node <- Nodes];
%% add_node(Node, ToAdd, Pid) ->
%%   #{pid := NPid} = Node,
%%   case NPid == Pid of
%%     true  ->
%%       maps:merge(Node, add_child(Node, ToAdd));
%%     false ->
%%       % maps:merge(Node, add_node(maps:get(children, Node), ToAdd, Pid))
%%       Node2 = add_node(maps:get(children, Node), ToAdd, Pid),
%%       io:format("Node2: ~p~n:", [Node2]),
%%       maps:merge(Node, Node2)
%%   end.
%%
%% add_child(Node, ToAdd) ->
%%   #{pid := NPid, children := NChildren} = Node,
%%   CNode = maps:put(parents, [NPid], ToAdd),
%%   PNode = maps:put(children, [CNode | NChildren], Node),
%%   PNode.

% -spec add_node(map(), map() | list(), pid()) -> map().
% (#{},#{},_) -> {'new',#{}}

add_node(Node, ToAdd) ->
  io:format("add_node ~p~n", [Node]),
  io:format("add_node ~p~n", [ToAdd]),
  #{pid := NPid} = Node,
  #{parent := TAPid} = ToAdd,
  case NPid == TAPid of
    true  ->
      Node2 = add_child(Node, ToAdd),
      {new, Node2};
    false ->
      Children2 = [],
      case maps:get(children, Node) of
        undefined ->
          not_found;
        [] ->
          not_found;
        CNs when is_list(CNs) ->
          [H|T] = CNs,
          X = add_node(H, ToAdd),
          case X of
            {new, NewNode} ->
              NewChildren = [Children2 | [NewNode | T]],
              UpdatedNode = maps:put(children, NewChildren, Node),
              {new, UpdatedNode};
            not_found ->
              % add_node(hd(T), ToAdd, Pid)
              not_found
          end

      end
  end.
%% add_node([], _ToAdd, _Pid) ->
%%   not_found;
%% add_node([HNode|TNodes], ToAdd, Pid) ->
%%   Res = add_node(HNode, ToAdd, Pid),
%%   case Res of
%%     not_found     ->
%%       add_node(TNodes, ToAdd, Pid);
%%     {ok, Updated} ->
%%
%%       HNode2 = maps:update(children, Updated, HNode),
%%       {ok, HNode2}
%%   end.

add_child(Node, ToAdd) ->
  #{children := NChildren} = Node,
  maps:put(children, [ToAdd | NChildren], Node).

%% display(Node) ->
%%   display(Node, 0).
%%
%% display(Node, Depth) ->
%%   #{pid := NPid, children := NChildren} = Node,
%%   print_line("|---", Depth),
%%   io:format("~p", [NPid]),
%%   case NChildren of
%%     [] -> ok;
%%     _ -> [display(NChild, Depth+1) || NChild <- NChildren]
%%   end.
%%
%% print_line(_Input, 0) ->
%%   ok;
%% print_line(Input, Count) ->
%%   io:format(Input, Count-1).


%% find_node(Nodes, ToFind) when is_list(Nodes) ->
%%   case Nodes of
%%     [] -> not_found;
%%     _  -> [find_node(Node, ToFind) || Node <- Nodes]
%%   end;
%% find_node(Node, ToFind) ->
%%   #{pid := NPid} = Node,
%%   case NPid == ToFind of
%%     false -> find_node(maps:get(children, Node), ToFind);
%%     true  -> Node
%%   end.




%%-----------------------------------------------------------------------------
%% @doc
%% Return the all the processes associated with the specified Pid.
%%
%% ==== Example Ouput ====
%%   ```
%%   #{
%%     group_leader => <0.79.0>,
%%     links => [<0.80.0>],
%%     monitored => [],
%%     monitored_by => [],
%%     pd_ancestors => [<0.80.0>],
%%     registered_name => plexo_srv_sup
%%   }
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec proc_vtx_nfo(pid()) -> map().

proc_vtx_nfo(Pid) ->
  proc_vtx_nfo(Pid, undefined).


-spec proc_vtx_nfo(pid(), pid() | atom()) -> map().

proc_vtx_nfo(Pid, ParentPid) ->

  PInfo = process_info(Pid),

  % Dereference all proccess related data...
  RN = proplists:get_value(registered_name, PInfo, undefined),
  GL = proplists:get_value(group_leader, PInfo, undefined),
  LNs = proplists:get_value(links, PInfo, []),
  MNs = proplists:get_value(monitored, PInfo, []),
  MNBs = proplists:get_value(monitored_by, PInfo, []),
  ProcDict = proplists:get_value(dictionary, PInfo),
  ANs =
    case proplists:get_value('$ancestors', ProcDict) of
      undefined     -> [];
      Ancestors -> [core_pid:get_as_pid(Proc) || Proc <- Ancestors]
    end,

  % Return map of data.
  #{
    pid => Pid,
    registered_name => RN,
    group_leader => GL,
    links => LNs,
    monitored => MNs,
    monitored_by => MNBs,
    pd_ancestors => ANs,
    parent => ParentPid,
    children => []
  }.


%%-----------------------------------------------------------------------------
%% @doc
%% Return the all the processes associated with the specified Pid.
%% @end
%%-----------------------------------------------------------------------------
-spec build_reg_proc_graph() -> [digraph:graph()].

build_reg_proc_graph() ->
  [build_reg_proc_graph(PidName) || PidName <- registered()].

%%-----------------------------------------------------------------------------
%% @doc
%% Return a Digraph of all the processes associated with the registered
%% process.
%% @end
%%-----------------------------------------------------------------------------
-spec build_reg_proc_graph(atom()) -> digraph:graph().

build_reg_proc_graph(Name) when is_atom(Name) ->
  build_proc_graph([whereis(Name)]).

%%-----------------------------------------------------------------------------
%% @doc
%% Return a Digraph of all the processes associated with the specified Pid.
%% @end
%%-----------------------------------------------------------------------------
-spec build_proc_graph(pid()|[pid()]) -> digraph:graph().

build_proc_graph(Pid) when is_pid(Pid) ->
   build_proc_graph([Pid]);
build_proc_graph(Pids) when is_list(Pids) ->
  Graph = digraph:new(),
  % Vertices = [ensure_pid_vtx(Graph, Pid) || Pid <- Pids],
  % _Edges = [add_pid_vtx_edges(Graph, Vertex) || Vertex <- Vertices],
  _Edges = [add_pid_vtx_edges(Graph, Pid) || Pid <- Pids],
  Graph.

% preorder
%%%============================================================================
%%% Private Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Add the specified VertexPid to the graph, along with all of the processes
%% 'associated pids'. See: extract_proc_nfo/1.
%%-----------------------------------------------------------------------------
-spec add_pid_vtx_edges(digraph:graph(), pid()) -> [digraph:edge()].

add_pid_vtx_edges(Graph, Pid) ->

  % Dereference all proccess related data...
  #{
    % pid := Pid,
    % registered_name := RN,
    group_leader := GL,
    links := LNs,
    monitored := MNs,
    monitored_by := MNBs,
    pd_ancestors := ANs
  } = proc_vtx_nfo(Pid),

  % Dereference all proccess related data...
  GrpLdr = add_pid_vtx_edges(Graph, Pid, GL, group_leader),
  Lnks   = [add_pid_vtx_edges(Graph, Pid, LN, links) || LN <- LNs],
  Mntr   = [add_pid_vtx_edges(Graph, Pid, MN, monitored) || MN <- MNs],
  MntrBy = [add_pid_vtx_edges(Graph, Pid, MNB, monitored_by) || MNB <- MNBs],
  Ancstr = [add_pid_vtx_edges(Graph, Pid, AN, pd_ancestors) || AN <- ANs],

  % lists:flatten([GrpLdr, Lnks, Mntr, MntrBy, Ancstr]).
  [GrpLdr, Lnks, Mntr, MntrBy, Ancstr].

%%-----------------------------------------------------------------------------
%% Add the specified VertexPid to the graph, along with all of the processes
%% 'associated pids'. See: extract_proc_nfo/1.
%%-----------------------------------------------------------------------------
-spec add_pid_vtx_edges(digraph:graph(), pid(), pid(), atom())
      -> [digraph:edge()].

add_pid_vtx_edges(Graph, PidVtx, AssocPid, LinkType) ->
  case LinkType of
    group_leader ->
      AssocPidVtx = ensure_pid_vtx(Graph, AssocPid),
      digraph:add_edge(Graph, PidVtx, AssocPidVtx, group_leader);
    links ->
      AssocPidVtx = ensure_pid_vtx(Graph, AssocPid),
      [
        digraph:add_edge(Graph, PidVtx, AssocPidVtx, link),
        digraph:add_edge(Graph, AssocPidVtx, PidVtx, link)
      ];
    monitored ->
      AssocPidVtx = ensure_pid_vtx(Graph, AssocPid),
      digraph:add_edge(Graph, PidVtx, AssocPidVtx, monitored);
    monitored_by ->
      AssocPidVtx = ensure_pid_vtx(Graph, AssocPid),
      digraph:add_edge(Graph, AssocPidVtx, PidVtx, monitored_by);
    pd_ancestors  ->
      AssocPidVtx = ensure_pid_vtx(Graph, AssocPid),
      digraph:add_edge(Graph, PidVtx, AssocPidVtx, pd_ancestors)
  end.

%%-----------------------------------------------------------------------------
%% Ensure the specified Pid is part of the specified Graph.
%%
%% If the Pid does not exist, add it is a Vertex and return it; otherwise
%% return the existing Vertex.
%%-----------------------------------------------------------------------------
-spec ensure_pid_vtx(digraph:graph(), pid()) -> digraph:vertex().

ensure_pid_vtx(Graph, Pid) ->
  case digraph:vertex(Graph, Pid) of
    {V, _Label} -> V;
    false       -> digraph:add_vertex(Graph, Pid, Pid)
  end.
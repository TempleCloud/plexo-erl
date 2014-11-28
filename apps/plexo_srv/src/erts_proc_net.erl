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
  proc_vtx_nfo/1,          % Get the currently 'loaded' apps.
  build_reg_proc_graph/0,
  build_reg_proc_graph/1,
  build_proc_graph/1
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

  % Assume the AppMaster is always defined as the GrooupLeader.
  AppMasterPid = GL,

  {AppMasterPid, AppSupMasterPid, AppSupPid}.

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
    pd_ancestors => ANs
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
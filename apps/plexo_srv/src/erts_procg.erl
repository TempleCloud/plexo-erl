%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% The {@module} module provides functions that interact with the ERTS to
%%% return information relating to Erlang 'applications' in the system.
%%%
%%% === Types ===
%%%
%%% The {@module} module exports several types ({@section app_tpl},
%%% {@section app_nfo}, {@section app_rec}) that are capable of representing
%%% ERTS application metadata.
%%%
%%% ==== app_tpl ====
%%%
%%% The native 3-tuple returned from ERTS module function calls.
%%%
%%% Example:
%%%   ```
%%%   {kernel,"ERTS  CXC 138 10","3.0.1"}
%%%   '''
%%%
%%% ==== app_nfo ====
%%%
%%% A 'map' based representation where the 'description' and 'version' values
%%% are 'binaries'.
%%%
%%% Suitable for conversion to hierarchical serializable representations
%%% such as 'json', 'xml', etc.
%%%
%%% Example:
%%%   ```
%%%   #{app_nfo => #{
%%%     description => <<"ERTS  CXC 138 10">>,
%%%     name => kernel,
%%%     version => <<"3.0.1">>
%%%     }
%%%   }
%%%   '''
%%%
%%% ==== app_rec ====
%%%
%%% A 'record' based representation where the 'description' and 'version'
%%% values are 'binaries'.
%%%
%%% Example:
%%%   ```
%%%   #{app_rec{kernel,<<"ERTS  CXC 138 10">>,<<"3.0.1">>}}
%%%   '''
%%% @end
%%%----------------------------------------------------------------------------
-module(erts_procg).
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
  extract_proc_nfo/1,          % Get the currently 'loaded' apps.
  build_proc_graph/1,
  build_reg_proc_graph/0
]).

%%%============================================================================
%%% Public Types
%%%============================================================================

%%%============================================================================
%%% Public Functions
%%%============================================================================

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
-spec extract_proc_nfo(pid()) -> map().

extract_proc_nfo(Pid) ->

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
-spec build_reg_proc_graph()
      -> tuple(digraph:graph(), [digraph:vertex()], [digraph:edge()]).

build_reg_proc_graph() ->
  RegProcs = [whereis(PidName) || PidName <- registered()],
  build_proc_graph(RegProcs).


%%-----------------------------------------------------------------------------
%% @doc
%% Return the all the processes associated with the specified Pid.
%% @end
%%-----------------------------------------------------------------------------
-spec build_proc_graph(pid()|[pid()])
      -> tuple(digraph:graph(), [digraph:vertex()], [digraph:edge()]).

build_proc_graph(Pid) when is_pid(Pid) ->
  build_proc_graph([Pid]);
build_proc_graph(Pids) when is_list(Pids) ->
  Graph = digraph:new(),
  Vertices = [ensure_pid_vtx(Graph, Pid) || Pid <- Pids],
  Edges = [add_pid_vtx_edges(Graph, Vertex) || Vertex <- Vertices],
  {Graph, Vertices, Edges}.


%%-----------------------------------------------------------------------------
%% Add the specified VertexPid to the graph, along with all of the processes
%% 'associated pids'. See: extract_proc_nfo/1.
%%-----------------------------------------------------------------------------
-spec add_pid_vtx_edges(digraph:graph(), pid()) -> [digraph:edge()].

add_pid_vtx_edges(Graph, Pid) ->

  % Dereference all proccess related data...
  #{
    % registered_name := RN,
    group_leader := GL,
    links := LNs,
    monitored := MNs,
    monitored_by := MNBs,
    pd_ancestors := ANs
  } = extract_proc_nfo(Pid),

  % Dereference all proccess related data...
  GrpLdr = add_pid_vtx_edges(Graph, Pid, GL, group_leader),
  Lnks   = [add_pid_vtx_edges(Graph, Pid, LN, links) || LN <- LNs],
  Mntr   = [add_pid_vtx_edges(Graph, Pid, MN, monitored) || MN <- MNs],
  MntrBy = [add_pid_vtx_edges(Graph, Pid, MNB, monitored_by) || MNB <- MNBs],
  Ancstr = [add_pid_vtx_edges(Graph, Pid, AN, pd_ancestors) || AN <- ANs],

  GrpLdr ++ Lnks ++ Mntr ++ MntrBy ++ Ancstr.

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
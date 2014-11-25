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
-module(erts_proc).
-author("Temple").

%%% Export all declared functions when TEST.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%%%============================================================================
%%% Public API
%%%============================================================================

%% -export_type([
%%   proc_tpl/0                % Erlang 'app' tuple type.
%% ]).

-export([
  get_procs_nfo/0,             % Get the currently 'loaded' apps.
  get_proc_nfo/1,              % Get the currently 'loaded' apps.
  get_proc_nfo_item/2,
  get_reg_procs_nfo/0,
  get_app_procs_nfo/0
]).

-export([
  build_proc_graph/1,
  extract_parent_procs/1        % Get the currently 'loaded' apps.
  % build_proc_tree/1
]).


-export([
  get_as_pid/1,
  is_local_pid/1,              % Get the currently 'loaded' apps.
  pid_to_tpl/1,                % Get the currently 'loaded' apps.
  tpl_to_pid/1
]).


%%%============================================================================
%%% Public Types
%%%============================================================================

%% [
%%   {registered_name,init},
%%   {current_function,{init,loop,1}},
%%   {initial_call,{otp_ring0,start,2}},
%%   {status,waiting},
%%   {message_queue_len,0},
%%   {messages,[]},
%%   {links,[<0.6.0>,<0.7.0>,<0.3.0>]},
%%   {dictionary,[]},
%%   {trap_exit,true},
%%   {error_handler,error_handler},
%%   {priority,normal},
%%   {group_leader,<0.0.0>},
%%   {total_heap_size,1974},
%%   {heap_size,1598},
%%   {stack_size,2},
%%   {reductions,3454},
%%   {
%%     garbage_collection,[
%%     {min_bin_vheap_size,46422},
%%     {min_heap_size,233},
%%     {fullsweep_after,65535},
%%     {minor_gcs,4}
%%     ]
%%   },
%%   {suspending,[]}
%% ]

%%%============================================================================
%%% Public Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Return the 'currently loaded applications' in the system as list of
%% 'app_nfo' map entities.
%%
%% See: i() / ni()
%% @end
%%-----------------------------------------------------------------------------
-spec get_procs_nfo() -> LoadedApps :: [any()].

get_procs_nfo() ->
  [get_proc_nfo(Pid) || Pid <- erlang:processes()].

get_proc_nfo(Pid) ->
  [core_util:proplist_to_map(erlang:process_info(Pid))].

get_proc_nfo_item(Pid, Item) ->
  [core_util:proplist_to_map(erlang:process_info(Pid, Item))].

get_reg_procs_nfo() ->
  [get_proc_nfo(erlang:whereis(PidName)) || PidName <- erlang:registered()].

%% [{current_function,{erl_eval,do_apply,6}},
%% {initial_call,{erlang,apply,2}},
%% {status,running},
%% {message_queue_len,0},
%% {messages,[]},
%% {links,[<0.26.0>]},
%% {dictionary,[]},
%% {trap_exit,false},
%% {error_handler,error_handler},
%% {priority,normal},
%% {group_leader,<0.25.0>},
%% {total_heap_size,987},
%% {heap_size,987},
%% {stack_size,24},
%% {reductions,1109},
%% {garbage_collection,[{min_bin_vheap_size,46422},
%% {min_heap_size,233},
%% {fullsweep_after,65535},
%% {minor_gcs,0}]},
%% {suspending,[]}]


%% extract_parent_procs(Pid) ->
%%   {dictionary, ProcDict} = process_info(Pid, dictionary),
%%   {'$ancestors', Procs} = lists:keyfind('$ancestors', 1, ProcDict),
%%   [get_as_pid(Proc) || Proc <- Procs].

extract_parent_procs(Pid) ->

  PInfo = process_info(Pid),
  RegisteredName = proplists:get_value(registered_name, PInfo),
  GroupLeader = proplists:get_value(group_leader, PInfo),
  Links = proplists:get_value(links, PInfo),
  Monitored = proplists:get_value(monitored, PInfo),
  MonitoredBy = proplists:get_value(monitored_by, PInfo),
  ProcDict = proplists:get_value(dictionary, PInfo),
  PID_Ancestors =
    case proplists:get_value('$ancestors', ProcDict) of
      undefined     -> undefined;
      DictAncestors -> [get_as_pid(Proc) || Proc <- DictAncestors]
    end,

  Map = #{
    registered_name => RegisteredName,
    group_leader => GroupLeader,
    links => Links,
    monitored => Monitored,
    monitored_by => MonitoredBy,
    pd_ancestors => PID_Ancestors
  },

  Map.


build_proc_graph(Pids) ->

  RegProcs = [{whereis(ProcName), ProcName} || ProcName <- registered()],
  Graph = digraph:new(),
  Vertices = [digraph:add_vertex(Graph, Pid, Pid) || {Pid, RegName} <- RegProcs].
  Edges = [add_edge(Graph, Pid) || Pid <- Vertices].

add_edge(Graph, Pid) ->

  #{
    registered_name := RegisteredName,
    group_leader := GroupLeader,
    links := Links,
    monitored := Monitored,
    monitored_by := MonitoredBy,
    pd_ancestors := PID_Ancestors
  } = extract_parent_procs(Pid),
  Edges = [digraph:add_edge(Graph, Pid, Pids, link) || Pids <- Links].

add_edge(Graph, Pid, Pids, link) ->
  

add_node(Graph, Pid) ->
  case digraph:vertex(Graph, Pid) of
    {V, _Label} -> V;
    false       -> digraph:add_vertex(Graph, Pid, Pid)
  end.

%% build_proc_tree(Pids) ->
%%   lists:foldl(fun update_proc_tree/2, #{}, Pids).
%%
%% update_proc_tree(Pid, ProcTreeMap) ->
%%   Parents = extract_parent_procs(Pid),
%%   Ancestor = find_parent({Pid, Parents}, ProcTreeMap),
%%   % maps:put(Pid, ProcTreeMap),
%%   nf.
%%
%% find_parent({Pid, Parents}, ProcTreeMap) ->
%%   nf.



get_app_procs_nfo() ->
  % [application:get_application(Pid) || Pid <- erlang:processes()].
  lists:foldl(fun collate/2, #{}, erlang:processes()).
collate(Pid, AccMap) ->
  case application:get_application(Pid) of
    {ok, AppName} -> addToMap(AppName, Pid, AccMap);
    _             -> AccMap
  end.
addToMap(AppName, Pid, AccMap) ->
  case maps:find(AppName, AccMap) of
    {ok, Pids}  -> maps:update(AppName, [Pid|Pids], AccMap);
    error       -> maps:put(AppName, [Pid], AccMap)
  end.


% https://github.com/erlang/otp/blob/maint/erts/emulator/beam/erl_term.h#L568

%% Nid: node id which is not arbitrary but the internal index for that node in dist_entry.
%%   (It is actually the atom slot integer for the node name.)
%% PIdx, process index which refers to the internal index in the proctab, (0 -> MAXPROCS).
%% C, Serial which increases every time MAXPROCS has been reached.


get_as_pid(Proc) when is_pid(Proc) ->
  Proc;
get_as_pid(Proc) when is_atom(Proc) ->
  erlang:whereis(Proc);
get_as_pid(_Proc) ->
  undefined.


pid_to_tpl(Pid) ->
  % Convert to string: "<0.32.0>"
  PidStr = pid_to_list(Pid),
  % Trim string: "0.32.0"
  TrimPidStr = lists:sublist(PidStr, 2, length(PidStr) - 2),
  % Convert to integer array: e.g. [0,32,0]
  [NId, PIdx, Ser] = [list_to_integer(S) || S <- string:tokens(TrimPidStr,[$.])],
  {NId, PIdx, Ser}.


tpl_to_pid({NId, PIdx, Ser}) ->
  % Assemble string: "<0.32.0>"
  PidStr = "<"
    ++ integer_to_list(NId) ++ "."
    ++ integer_to_list(PIdx) ++ "."
    ++ integer_to_list(Ser)
    ++ ">",
  list_to_pid(PidStr).


is_local_pid(Pid) ->
  case pid_to_tpl(Pid) of
    {0, _, _} -> true;
    {_, _, _} -> false
  end.
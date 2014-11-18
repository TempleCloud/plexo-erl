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
  get_procs/0            % Get the currently 'loaded' apps.
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
%% @end
%%-----------------------------------------------------------------------------
-spec get_procs() -> LoadedApps :: [any()].

% i() / ni()
get_procs() ->
  [core_util:proplist_to_map(erlang:process_info(Pid))
    || Pid <- erlang:processes()].

get_proc(Pid) ->
  [core_util:proplist_to_map(erlang:process_info(Pid))].

% https://github.com/erlang/otp/blob/maint/erts/emulator/beam/erl_term.h#L568

%% Nid: node id which is not arbitrary but the internal index for that node in dist_entry.
%%   (It is actually the atom slot integer for the node name.)
%% PIdx, process index which refers to the internal index in the proctab, (0 -> MAXPROCS).
%% C, Serial which increases every time MAXPROCS has been reached.


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

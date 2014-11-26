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

-export([
  get_proc_nfo/1,              % Get all nfo of the specified process.
  get_proc_nfo/2,              % Get the selected nfo of the specified process.
  get_all_proc_nfo/0,          % Get all nfo of all running processes.
  get_reg_proc_nfo/0          % Get all nfo of all registered processes.
]).

%%%============================================================================
%%% Public Types
%%%============================================================================

%%%============================================================================
%%% Public Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Return the {@link proc_info/1} of the specified process as a map structure.
%%
%% ==== Example Ouput ====
%%   ```
%%   #{
%%     current_function => {erts_proc,get_proc_nfo,1},
%%     dictionary => <<>>,
%%     error_handler => error_handler,
%%     garbage_collection => #{fullsweep_after => 65535,
%%     min_bin_vheap_size => 46422,
%%     min_heap_size => 233,
%%     minor_gcs => 4},
%%     group_leader => <0.25.0>,
%%     heap_size => 376,
%%     initial_call => {erlang,apply,2},
%%     links => [<0.26.0>],
%%     message_queue_len => 0,
%%     messages => <<>>,
%%     priority => normal,
%%     reductions => 565,
%%     stack_size => 25,
%%     status => running,
%%     suspending => <<>>,
%%     total_heap_size => 752,
%%     trap_exit => false
%%   }
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec get_proc_nfo(Pid :: pid()) -> ProcNfo :: map().

get_proc_nfo(Pid) ->
  core_util:proplist_to_map(erlang:process_info(Pid)).

%%-----------------------------------------------------------------------------
%% @doc
%% Return the {@link proc_info/2} of the specified process attribute as a map
%% structure.
%%
%% ==== Example Ouput ====
%%   ```
%%   #{trap_exit => false}
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec get_proc_nfo(Pid :: pid(), Item :: atom()) -> ProcNfo :: map().

get_proc_nfo(Pid, Item) ->
  core_util:proplist_to_map([process_info(Pid, Item)]).

%%-----------------------------------------------------------------------------
%% @doc
%% Return the {@link proc_info/1} of *ALL* processes running on the node
%% as a list of map structure.
%%
%% NB: Use with care with large numbers of processes.
%% @end
%%-----------------------------------------------------------------------------
-spec get_all_proc_nfo() -> AllProcNfo :: [map()].

get_all_proc_nfo() ->
  [get_proc_nfo(Pid) || Pid <- processes()].

%%-----------------------------------------------------------------------------
%% @doc
%% Return the {@link proc_info/1} of *REGISTERED* processes running on the
%% system as a list of map structure.
%%
%% NB: Use with care with large numbers of processes.
%% @end
%%-----------------------------------------------------------------------------
-spec get_reg_proc_nfo() -> AllProcNfo :: [map()].

get_reg_proc_nfo() ->
  [get_proc_nfo(whereis(PidName)) || PidName <- registered()].


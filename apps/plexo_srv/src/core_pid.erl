%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% The {@module} module provides functions that help with operations on
%%% Pid types.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(core_pid).
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
  get_as_pid/1,                % Get the Pid type from a representation.
  is_local_pid/1,              % Determine if this is a local process.
  get_app_pids/1,              % Get Pids for the specified application.
  get_all_app_pids/0           % Get Pids for each running application.
]).

%%%============================================================================
%%% Public Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Return the Pid process corresponding to the specified input. Inputs include
%% a valid OTP application atom name, a 3-tuple, or a list/binary
%% representation.
%%
%% If the process cannot be resolved; then undefined is returned.
%%
%% ==== Example Inputs ====
%%
%% Application: ```sasl, kernel, crypto'''
%% 3-Tuple    : ```{0, 32, 0}'''
%% String     : ```"<0.32.0>"'''
%% Binary     : ```<<"<0.32.0>">>'''
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_as_pid(any()) -> pid() | undefined.

get_as_pid(Proc) when is_pid(Proc) ->
  Proc;
get_as_pid(Proc) when is_atom(Proc) ->
  whereis(Proc);
get_as_pid({NId, PIdx, Ser}) ->
  tpl_to_pid({NId, PIdx, Ser});
get_as_pid(Proc) when is_list(Proc) ->
  list_to_pid(Proc);
get_as_pid(Proc) when is_binary(Proc) ->
  get_as_pid(binary_to_list(Proc));
get_as_pid(_Proc) ->
  undefined.

%%-----------------------------------------------------------------------------
%% @doc
%% Convert the specified Pid to a 3-tuple integer representation.
%% @end
%%-----------------------------------------------------------------------------
-spec pid_to_tpl(pid()) -> tuple(integer(), integer(), integer()).

pid_to_tpl(Pid) ->
  % Convert to string: "<0.32.0>"
  PidStr = pid_to_list(Pid),
  % Trim string: "0.32.0"
  TrimPidStr = lists:sublist(PidStr, 2, length(PidStr) - 2),
  % Convert to integer array: e.g. [0,32,0]
  [NId, PIdx, Ser] = [list_to_integer(S) || S <- string:tokens(TrimPidStr,[$.])],
  {NId, PIdx, Ser}.

%%-----------------------------------------------------------------------------
%% @doc
%% Convert the specified 3-tuple integer Pid representation to the Pid.
%% @end
%%-----------------------------------------------------------------------------
-spec tpl_to_pid(tuple(integer(), integer(), integer())) -> pid().

tpl_to_pid({NId, PIdx, Ser}) ->
  % Assemble string: "<0.32.0>"
  PidStr = "<"
    ++ integer_to_list(NId) ++ "."
    ++ integer_to_list(PIdx) ++ "."
    ++ integer_to_list(Ser)
    ++ ">",
  list_to_pid(PidStr).

%%-----------------------------------------------------------------------------
%% @doc
%% Return true if the specified Pid is local to this node.
%% @end
%%-----------------------------------------------------------------------------
-spec is_local_pid(pid()) -> boolean().

is_local_pid(Pid) ->
  case pid_to_tpl(Pid) of
    {0, _, _} -> true;
    {_, _, _} -> false
  end.

%%-----------------------------------------------------------------------------
%% @doc
%% Return a map of the 'application processes' running in the system; keyed on
%% the application the processes are asociated with.
%%
%% ==== Example Ouput ====
%%   ```
%%   #{
%%     cowboy => [<0.76.0>,<0.75.0>,<0.74.0>,<0.73.0>],
%%     plexo_srv => [<0.184.0>,<0.80.0>,<0.79.0>]
%%     ...
%%     app_name => [Pid1, Pid2,...,PidN]
%%   }
%%   '''
%%
%% NB: Use with care with large numbers of processes.
%% NB: See: i() / ni()
%% @end
%%-----------------------------------------------------------------------------
-spec get_all_app_pids() -> AppProcs :: map().

get_all_app_pids() ->
  lists:foldl(fun collate/2, #{}, processes()).

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

%%-----------------------------------------------------------------------------
%% @doc
%% Return a the list of known Pids associated with the specified OTP
%% application.
%% @end
%% @todo Just create necessary Pid list.
%%-----------------------------------------------------------------------------
-spec get_app_pids(AppName :: atom()) -> AppProcs :: list(pid()).

get_app_pids(AppName) ->
  maps:get(AppName, get_all_app_pids()).

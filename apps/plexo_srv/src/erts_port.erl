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
-module(erts_port).
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
  get_ports_nfo/0,            % Get the currently 'loaded' apps.
  get_port_nfo/1
]).


%%%============================================================================
%%% Public Types
%%%============================================================================


%%%============================================================================
%%% Public Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Return the 'currently loaded applications' in the system as list of
%% 'app_nfo' map entities.
%%
%% Example:
%%   ```
%%   [
%%     {name,"efile"},
%%     {links,[<0.3.0>]},
%%     {id,0},
%%     {connected,<0.3.0>},
%%     {input,53},
%%     {output,3},
%%     {os_pid,undefined}
%%   ]
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec get_ports_nfo() -> PortNfos :: [any()].

get_ports_nfo() ->
  [get_port_nfo(Port) || Port <- erlang:ports()].

%%-----------------------------------------------------------------------------
%% @doc
%% Return the 'currently loaded applications' in the system as list of
%% 'app_nfo' map entities.
%%
%% Example:
%%   ```
%%   [
%%     {name,"efile"},
%%     {links,[<0.3.0>]},
%%     {id,0},
%%     {connected,<0.3.0>},
%%     {input,53},
%%     {output,3},
%%     {os_pid,undefined}
%%   ]
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec get_port_nfo(Port :: port()) -> PortNfo :: any().

get_port_nfo(Port) ->
  erlang:port_info(Port).
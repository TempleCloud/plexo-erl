%%%-------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% The {@module} contains various of utility functions.
%%% @end
%%%-------------------------------------------------------------------
-module(core_util).
-author("Temple").

%%% Export all declared functions when TEST.
-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
  -compile(export_all).
-endif.

%%%============================================================================
%% Public API
%%%============================================================================
-export([
  to_atom_key_map/1            % Convert a nested map to have atomic keys.
]).

%%%============================================================================
%% Public Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Take an input Map and convert any 'binary map keys' to 'atom map keys'.
%%
%% ==== Example Input ====
%%   ```
%%   #{<<"app_nfo">> => #{
%%     <<"description">> => <<"ERTS  CXC 138 10">>,
%%     <<"name">> => <<"kernel">>,
%%     <<"version">> => <<"3.0.1">>
%%     }
%%   }
%%   '''
%%
%% ==== Example Output ====
%%   ```
%%   #{app_nfo => #{
%%     description => <<"ERTS  CXC 138 10">>,
%%     name => <<"kernel">>,
%%     version => <<"3.0.1">>
%%     }
%%   }
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec to_atom_key_map(map()) -> map().

to_atom_key_map(Input) ->
  case Input of
    % Handle single map - convert all binary keys to atoms.
    #{}     -> atomise_map(Input);
    % Handle collection of items.
    [_H|_T] -> [atomise_map(X) || X <- Input];
    % Ignore all other types.
    X       -> X
  end.

atomise_map(Map) when is_map(Map) ->
  maps:fold(fun atomise_kv/3, #{}, Map).

atomise_kv(K, V, Map) when is_binary(K), is_map(V) ->
  maps:put(binary_to_atom(K, utf8), atomise_map(V), Map);
atomise_kv(K, V, Map) when is_binary(K) ->
  maps:put(binary_to_atom(K, utf8), V, Map).

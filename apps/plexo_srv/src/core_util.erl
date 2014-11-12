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
  to_atom_key_map/1,            % Convert a nested map to have atomic keys.
  proplist_to_map/1,            % Convert a nested proplist to a nested map.
  is_proplist/1                 % Determine if the input is a proplist.
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
-spec proplist_to_map(list(proplists:property())) -> map().

proplist_to_map(Input) when is_list(Input) ->
  proplist_to_map(Input, #{}).

proplist_to_map(Input, AccMap) when is_list(Input) ->
  lists:foldl(fun proplist_item_to_map/2, AccMap, Input).


proplist_item_to_map({Key, Val}, Map) when is_list(Val) ->
  case io_lib:char_list(Val) of
    true ->
      maps:put(Key, list_to_binary(Val), Map);
    false ->
      case is_proplist(Val) of
        true  ->
          maps:put(Key, proplist_to_map(Val), Map);
        false ->
          maps:put(Key, Val, Map)
      end
  end;
proplist_item_to_map({Key, Val}, Map) ->
  case is_proplist_item(Val) of
    true  -> maps:put(Key, proplist_item_to_map(Val, #{}), Map);
    false -> maps:put(Key, Val, Map)
  end.


is_proplist(List) ->
  is_list(List) andalso
    lists:all(
      fun(
        {_, _}) -> true;
        (_)     -> false
      end,
      List).

is_proplist_item(Item) ->
  case Item of
    {_,_} -> true;
    _     -> false
  end.

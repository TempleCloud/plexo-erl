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
  is_property/1                 % Determine if the input is a proplist.
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
%% Take an input proplist and convert it into a single map where the key-vlaue
%% (kv) pairs in the proplist become kv pairs in the resultant map. Furthermore,
%% if the value of a kv pair is another proplist then recursively process that
%% list and any child proplists. If any value is a charlist then it is
%% converted into a binary. If a value is a general list or single value it is
%% simply stored.
%%
%% Mixed collections, or values not conforming to the above will probably
%% result in an error.
%%
%% ==== Example Input ====
%%   ```
%%   [{description,"SASL  CXC 138 11"},
%%     {id,[]},
%%     {vsn,"2.4.1"},
%%     {modules,[sasl,alarm_handler,release_handler]},
%%     {maxP,infinity},
%%     {maxT,infinity},
%%     {included_applications,[]},
%%     {applications,[kernel,stdlib]},
%%     {env,[{errlog_type,all},
%%     {included_applications,[]},
%%     {sasl_error_logger,tty}]},
%%     {mod,{sasl,[]}},
%%     {start_phases,undefined}
%%   ]
%%   '''
%%
%% ==== Example Output ====
%%   ```
%%   #{applications => [kernel,stdlib],
%%     description => <<"SASL  CXC 138 11">>,
%%     env => #{errlog_type => all,
%%     sasl_error_logger => tty},
%%     id => <<>>,
%%     included_applications => <<>>,
%%     maxP => infinity,
%%     maxT => infinity,
%%     mod => #{sasl => <<>>},
%%     modules => [sasl,alarm_handler,release_handler],
%%     start_phases => undefined,
%%     vsn => <<"2.4.1">>
%%   }
%%   '''
%% @TODO Make this method more robust.
%% @end
%%-----------------------------------------------------------------------------
-spec proplist_to_map(list(proplists:property())) -> map().

proplist_to_map(Input) when is_list(Input) ->
  proplist_to_map(Input, #{});
proplist_to_map(Input)  ->
  proplist_to_map([Input], #{}).


-spec proplist_to_map(list(proplists:property()), map()) -> map().

proplist_to_map(Input, AccMap) when is_list(Input) ->
  lists:foldl(fun add_property/2, AccMap, Input).

%%-----------------------------------------------------------------------------
%% @doc
%% Add the specified property to the specified map and return the new map.
%%
%% If the list value is a 'charlist' then convert it to a binary and add it.
%% If the list value is a 'proplist' then recursively process it and add it.
%% If the list value is any other list the simply add it.
%% @end
%%-----------------------------------------------------------------------------
-spec add_property(proplists:property(), map()) -> map().

% Handle 'list' values.
add_property({Key, Val}, Map) when is_list(Val) ->
  case io_lib:char_list(Val) of
    true ->
      % Convert 'charlist' to binary.
      maps:put(Key, list_to_binary(Val), Map);
    false ->
      case is_proplist(Val) of
        true  ->
          % Recursively process 'proplist'.
          maps:put(Key, proplist_to_map(Val), Map);
        false ->
          % Add standard lists.
          maps:put(Key, Val, Map)
      end
  end;
% Handle 'property' values.
add_property({Key, Val}, Map) ->
  case is_property(Val) of
    true  -> maps:put(Key, add_property(Val, #{}), Map);
    false -> maps:put(Key, Val, Map)
  end.

%%-----------------------------------------------------------------------------
%% @doc
%% If every element in the specified list is a {@link proplists:property()}
%% then return true; else false.
%% @end
%%-----------------------------------------------------------------------------
-spec is_proplist(list(proplists:property())) -> boolean().

is_proplist(List) ->
  is_list(List) andalso
    lists:all(
      fun(
        {_, _}) -> true;
        (_)     -> false
      end,
      List).

%%-----------------------------------------------------------------------------
%% @doc
%% If every element in the specified item is a {@link proplists:property()}
%% then return true; else false.
%% @end
%%-----------------------------------------------------------------------------
-spec is_property(proplists:property()) -> boolean().

is_property(Item) ->
  case Item of
    {_,_} -> true;
    _     -> false
  end.

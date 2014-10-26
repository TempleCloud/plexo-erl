%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @reference The <a href="https://github.com/talentdeficit/jsx">JSX</a>
%%% github repo.
%%% @reference The <a href="https://github.com/talentdeficit/jsxn">JSXN</a>
%%% github repo.
%%%
%%% @doc
%%% The {@module} module provides centralised Erlang Term / JSON conversion
%%% routines.
%%%
%%% ==== Notes ====
%%%
%%% <ol>
%%%   <li>
%%%     The current implementation uses the
%%%     <a href="https://github.com/talentdeficit/jsx">JSX</a> and
%%%     <a href="https://github.com/talentdeficit/jsx">JSXN</a> and
%%%     libraries to perform the marshalling operations.
%%%   </li>
%%% </ol>
%%% @end
%%%----------------------------------------------------------------------------
-module(core_json).
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
  to_json/1,
  from_json/1
]).

%%%============================================================================
%% Public Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Encode the specified 'Erlang term' into a JSON encoded binary. All 'string'
%% entities in the specified term must be binary encoded.
%%
%% ==== Example Input (Map Term) ====
%%   ```
%%   #{<<"app_nfo">> => #{
%%     <<"description">> => <<"ERTS  CXC 138 10">>,
%%     <<"name">> => <<"kernel">>,
%%     <<"version">> => <<"3.0.1">>
%%     }
%%   }
%%   '''
%%
%% ==== Example Output (JSON Binary) ====
%%   ```
%%   <<"{
%%     \"app_nfo\":{
%%       \"description\":\"ERTS  CXC 138 10\",
%%       \"name\":\"kernel\",
%%       \"version\":\"3.0.1\"
%%     }
%%   }">>
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec to_json(term()) -> binary().
to_json(ErlTerm) ->
  io:format("Encoding: ~p ~n", [ErlTerm]),
  Json = jsxn:encode(ErlTerm),
  io:format("Encoded: ~p ~n", [Json]),
  Json.

%%-----------------------------------------------------------------------------
%% @doc
%% Decode the specified JSON encoded binary into an 'Erlang term'. All 'string'
%% entities in the specified term will remain binary encoded.
%%
%% ==== Example Input (JSON Binary) ====
%%   ```
%%   <<"{
%%     \"app_nfo\":{
%%       \"description\":\"ERTS  CXC 138 10\",
%%       \"name\":\"kernel\",
%%       \"version\":\"3.0.1\"
%%     }
%%   }">>
%%   '''
%%
%% ==== Example Output (Map Term) ====
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
-spec from_json(binary()) -> term().
from_json(JsonBin) ->
  io:format("Decoding: ~p ~n", [JsonBin]),
  JsonMap = jsxn:decode(JsonBin),
  io:format("Decoded: ~p ~n", [JsonMap]),
  JsonMap.




%% %%%============================================================================
%% %% Private Functions
%% %%%============================================================================
%%
%% to_atom_key_map(JsonTerm) ->
%%   case JsonTerm of
%%     []      -> [];
%%     [_H|_T] -> [atomise_map(JsonMap) || JsonMap <- JsonTerm];
%%     #{}     -> atomise_map(JsonTerm)
%%   end.
%%
%% atomise_map(JsonMap) when is_map(JsonMap) ->
%%   maps:fold(fun atomise_kv/3, #{}, JsonMap).
%%
%% atomise_kv(K, V, Map) when is_binary(K), is_map(V) ->
%%   maps:put(binary_to_atom(K, utf8), atomise_map(V), Map);
%% atomise_kv(K, V, Map) when is_binary(K) ->
%%   maps:put(binary_to_atom(K, utf8), V, Map).
%%
%%
%%
%% %% to_binary_key_map(JsonTerm) ->
%% %%   case JsonTerm of
%% %%     #{} -> binarise_map(JsonTerm);
%% %%     []  -> [binarise_map(JsonMap) || JsonMap <- JsonTerm]
%% %%   end.
%% %%
%% %% binarise_map(JsonMap) when is_map(JsonMap) ->
%% %%   maps:fold(fun binarise_kv/3, #{}, JsonMap).
%% %%
%% %% binarise_kv(K, V, Map) when is_atom(K), is_map(V) ->
%% %%   maps:put(binary_to_atom(K, utf8), binarise_map(V), Map);
%% %% binarise_kv(K, V, Map) when is_atom(K) ->
%% %%   maps:put(binary_to_atom(K, utf8), V, Map).
%%
%%
%%
%% % % @todo Remove - to_atom_key_map v2.
%% %%
%% %% to_atom_key_map(JsonMap) ->
%% %%   maps:fold(fun (K, V, Map) -> maps:put(binary_to_atom(K, utf8), V, Map) end , #{} ,JsonMap).
%%
%%
%% % % @todo Remove - to_atom_key_map v1.
%% %%
%% %% -spec to_atom_key_map(map()) -> map().
%% %% to_atom_key_map(JsonMap) ->
%% %%   lists:foldl(fun build/2, #{}, [atomise(K, JsonMap) || K <- maps:keys(JsonMap)]).
%% %%
%% %% atomise(Key, JsonMap) ->
%% %%   {binary_to_atom(Key, utf8), maps:get(Key, JsonMap)}.
%% %%
%% %% build({K,V}, Map) ->
%% %%   maps:put(K, V, Map).
%%
%%
%%



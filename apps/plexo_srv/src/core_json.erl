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
to_json(Term) ->
  io:format("Encoding: ~p ~n", [Term]),
  Json = jsxn:encode(Term),
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
from_json(Json) ->
  io:format("Decoding: ~p ~n", [Json]),
  Map = jsxn:decode(Json),
  io:format("Decoded: ~p ~n", [Map]),
  Map.




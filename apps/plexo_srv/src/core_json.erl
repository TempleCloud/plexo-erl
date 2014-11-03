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
  to_json/1,                   % Convert a JSON term to a JSON binary.
  from_json/1                  % Convert a JSON binary to a JSON term.
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
-spec to_json(map() | list()) -> binary().

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
-spec from_json(binary()) -> map() | list().

% Handle binary() input...
from_json(Json) when is_binary(Json) ->
  jsxn:decode(Json);
% Handle list(char()) input...
from_json(Json) when is_list(Json) ->
  jsxn:decode(list_to_binary(Json)).





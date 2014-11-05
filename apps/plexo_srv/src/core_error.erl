%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @reference The <a href="https://github.com/talentdeficit/jsx">JSX</a>
%%% github repo.
%%% @reference The <a href="https://github.com/talentdeficit/jsxn">JSXN</a>
%%% github repo.
%%%
%%% @doc
%%% The {@module} module provides functions relating to error response returned
%%% from the Plexo service.
%%% @end
%%%----------------------------------------------------------------------------
-module(core_error).
-author("Temple").

%%%============================================================================
%% Public API
%%%============================================================================

-export([
  gen_json_rs/1                 % Generate a JSON error response.
]).

%%%============================================================================
%% Public Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%%
%%
%% ==== Example Input ====
%%   ```
%%
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec gen_json_rs(ErrorMsg :: binary()) -> Response :: binary().

gen_json_rs(ErrMsg) ->
  % core_json:to_json(#{<<"result">> => ErrMsg}).
  core_json:to_json(#{error => ErrMsg}).
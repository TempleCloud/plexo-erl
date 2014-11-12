%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% The {@module} module provides centralised methods relating to HTTP
%%% operations.
%%% @end
%%%----------------------------------------------------------------------------
-module(util_inet).
-author("Temple").

%%%============================================================================
%% Public API
%%%============================================================================

-export([
  auth_header/2                % Create Http Basic Auth Header.
]).

%%%============================================================================
%% Public Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Create a 'HTTP Basic Auth' header from the specified User and Passwd.
%% @end
%%-----------------------------------------------------------------------------
-spec auth_header(iolist(), iolist()) -> {iolist(), iolist()}.

auth_header(User, Passwd) ->
  % HTTP Basic auth uses Base64 encoding over the '${User}:${Passwd}' string.
  Encoded = base64:encode_to_string(lists:append([User,":",Passwd])),
  {"Authorization","Basic " ++ Encoded}.
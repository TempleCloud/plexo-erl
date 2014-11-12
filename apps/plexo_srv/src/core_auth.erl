%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% The {@module} module provides centralised routines relating to
%%% authentication and authorisation.
%%%
%%% ==== Notes ====
%%%
%%% What authentication mechanisms should I provide? For example: form-based,
%%% token-based (in the URL or a cookie), HTTP basic, HTTP digest, SSL
%%% certificate or any other form of authentication?
%%%
%%% This seems cool and relevant...
%%% https://auth0.com/blog/2014/01/07/angularjs-authentication-with-cookies-vs-token/
%%% @end
%%%----------------------------------------------------------------------------
-module(core_auth).
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
  restful_auth/1,               % Do a restful authetication and authorisation.
  do_auth/1,                    % Do a simple authentication.
  determine_realm/1,            % Determine the realm for this authentication.
  http_basic_realm_hdr/1        % Create a basic realm challenge HTTP response.
]).

%%%============================================================================
%% Public Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Perform a 'restful' authentication and authorisation check with respect to
%% the specified RestAction.
%%
%% Authetication of identity should proceed by a standard means (such as HTTP
%% Basic, Token, JSON Web Token, etc.).
%%
%% Authoriation over a specic resource should used some kind of ACL based
%% protection relative to the RestAction object.
%%
%% Return true if both operations succeed; false otherwise.
%%
%% ==== Example Input ====
%%   ```
%%   #{
%%     auth => #{
%%       user => <<"Temple">>,
%%       pass => <<"Wibble2Wobble">>,
%%       type => <<"basic">>
%%     },
%%     action => #{
%%       version => 'HTTP/1.1'
%%       url => <<"http://localhost:8877/api/app/sasl">>,
%%       method => <<"PUT">>,
%%       path => <<"/api/app/sasl">>,
%%       pathInfo => undefined,
%%       urlQuery => <<>>,
%%     },
%%     host => #{
%%       url => <<"http://localhost:8877">>,
%%       name => <<"localhost">>,
%%       port => 8877,
%%       info => undefined
%%     },
%%     peer => #{
%%       ip => {127,0,0,1},
%%       port => 51591
%%     }
%%   }
%%   '''
%% @TODO Replace static credential mechanism with proper user/password lookup.
%% @end
%%-----------------------------------------------------------------------------
-spec restful_auth(Req :: core_rest:rest_action()) -> true | false.

restful_auth(RestAction) ->
  #{auth := AuthNfo} = RestAction,
  case AuthNfo of
    undefined ->
      false;
    _ ->
      #{user := User, pass :=  Passwd, type := Type} = AuthNfo,
      do_auth({User, Passwd, Type})
  end.

%%-----------------------------------------------------------------------------
%% @doc
%% Perform a 'restful' authentication check with respect to the specified
%% AuthType.
%%
%% Return true if the user is authenticated; false otherwise.
%%
%% ==== Example Input ====
%%   ```
%%   {<<"User">>, <<"Password">>, <<"AuthType">>}
%%   '''
%% @TODO Replace static credential mechanism with proper user/password lookup.
%% @end
%%-----------------------------------------------------------------------------
do_auth({User, Passwd, Type}) ->
  case {User, Passwd, Type} of
    % For now just hardcode some basic authentication.
    {<<"Temple">>, <<"Wibble2Wobble">>, <<"basic">>} ->
      lager:info("Authenticated User:~p, Pass~p~n", [User, Passwd]),
      true;
    _ ->
      lager:info("Invalid User:~p, Pass~p~n", [User, Passwd]),
      false
  end.

%%-----------------------------------------------------------------------------
%% @doc
%% Return a binary string that denotes the security realm with respect to the
%% specified RestAction.
%% @TODO Replace static realm mechanism with proper lookup.
%% @end
%%-----------------------------------------------------------------------------
-spec determine_realm(RestAction :: any()) -> Realm :: binary().

determine_realm(_RestAction) ->
  <<"plexo">>.

%%-----------------------------------------------------------------------------
%% @doc
%% A utility method to create an HTTP-Basic binary string header.
%% @end
%%-----------------------------------------------------------------------------
-spec http_basic_realm_hdr(RestAction :: any()) -> RealmHdr :: binary().

http_basic_realm_hdr(RestAction) ->
  % e.g. <<"Basic realm=\"plexo\"">>.
  Realm = determine_realm(RestAction),
  <<<<"Basic realm=\"">>/binary, Realm/binary, <<"\"">>/binary>>.
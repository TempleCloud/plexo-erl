%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @reference The <a href="https://github.com/talentdeficit/jsx">JSX</a>
%%% github repo.
%%% @reference The <a href="https://github.com/talentdeficit/jsxn">JSXN</a>
%%% github repo.
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
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(core_auth).
-author("Temple").

%%%============================================================================
%% Public API
%%%============================================================================

-export([
  restful_auth/1,               % Convert a JSON term to a JSON binary.
  determine_realm/1,            % Convert a JSON term to a JSON binary.
  http_basic_realm_hdr/1        % Convert a JSON term to a JSON binary.
]).

%%%============================================================================
%% Public Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Perform a 'restful' authentication and authorisation check with respect to
%% the specified RestAction.
%%
%% Authetication of identitiy should proceed by a standard means (such as HTTP
%% Basic, Token, JSON Web Token, etc.).
%%
%% Authoriation over a specic resource should used some kind of ACL based
%% protection relative to the RESTAction object.
%%
%% Return true if both operations succeed; false otherwise.
%%
%% ==== Example RestAction ====
%%   ```
%%   #{
%%     host => #{
%%       info => undefined,
%%       name => <<"localhost">>,
%%       port => 8877,
%%       url => <<"http://localhost:8877">>
%%     },
%%     peer => #{
%%       ip => {127,0,0,1},
%%       port => 51591
%%     },
%%     request => #{
%%       auth => #{
%%         passwd => <<"Wibble2Wobble">>,
%%         type => <<"basic">>,
%%         user => <<"Temple">>
%%       },
%%       method => <<"PUT">>,
%%       path => <<"/api/app/sasl">>,
%%       pathInfo => undefined,
%%       queryString => <<>>,
%%       url => <<"http://localhost:8877/api/app/sasl">>,
%%       version => 'HTTP/1.1'
%%     }
%%   }
%%   '''
%% @TODO Replace static credential mechanism with proper user/password lookup.
%% @end
%%-----------------------------------------------------------------------------
-spec restful_auth(Req :: map()) -> true | false.

restful_auth(RestAction) ->

  % For now just hardcode this...
  #{request := #{auth := #{user := User, passwd :=  Passwd}}} = RestAction,
  case {User, Passwd} of
    {<<"Temple">>, <<"Wibble2Wobble">>} ->
      true;
    _ ->
      false
  end.

%%-----------------------------------------------------------------------------
%% @doc
%% Return a binary string that denotes the security realm with respect to the
%% specified RestAction.
%% @TODO Replace static realm mechanism with proper ookup.
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
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
-module(core_auth).
-author("Temple").

%%%============================================================================
%% Public API
%%%============================================================================

-export([
  determine_realm/1,            % Convert a JSON term to a JSON binary.
  basic_realm_hdr/1,            % Convert a JSON term to a JSON binary.
  restful_auth/1                % Convert a JSON term to a JSON binary.
]).

%%%============================================================================
%% Public Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%%-----------------------------------------------------------------------------
-spec determine_realm(RestAction :: any()) -> Realm :: binary().

determine_realm(_RestAction) ->
  <<"plexo">>.

%%-----------------------------------------------------------------------------
%% @doc
%%-----------------------------------------------------------------------------
-spec basic_realm_hdr(RestAction :: any()) -> RealmHdr :: binary().

basic_realm_hdr(RestAction) ->
  % e.g. <<"Basic realm=\"plexo\"">>.
  Realm = determine_realm(RestAction),
  <<<<"Basic realm=\"">>/binary, Realm/binary, <<"\"">>/binary>>.

%%-----------------------------------------------------------------------------
%% @doc
%%
%%
%%
%% ==== Example Ouput ====
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
%% @end
%%
%% @Todo Replace static credential mechanism with proper user/password lookup.
%%-----------------------------------------------------------------------------
-spec restful_auth(Req :: map()) -> true | false.

restful_auth(RestAction) ->

  #{request := #{auth := #{user := User, passwd :=  Passwd}}} = RestAction,
  case {User, Passwd} of
    {<<"Temple">>, <<"Wibble2Wobble">>} ->
      true;
    _ ->
      false
  end.

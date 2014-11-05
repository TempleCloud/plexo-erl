%%-----------------------------------------------------------------------------
%% @doc
%% An abstraction upon an HTTP/REST call.
%%
%% Defines some map based types that can be used to capture parameters relating
%% to an HTTP/REST call associated with performing a speific action upon a
%% resource or collection of resources.
%%
%% Can be used for a vareity of tasks such as authentication, ACL based
%% authorisation, action auditingm etc.
%%
%% ==== Example RestAction ====
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
%% @end
%%-----------------------------------------------------------------------------
-module(core_rest).
-author("Temple").

%%%============================================================================
%% Public API
%%%============================================================================

-export_type([
  rest_action/0,               % An HTTP/REST action.
  auth/0,                      % Authentication parameters.
  action/0,                    % Action parameters.
  host/0,                      % Host parameters.
  peer/0                       % Peer parameters.
]).

%%%============================================================================
%%% Public Types
%%%============================================================================

%%-----------------------------------------------------------------------------
-type rest_action() :: #{
  auth => auth(),
  action => action(),
  host => host(),
  peer => peer()
}.

%%-----------------------------------------------------------------------------
-type action() :: #{
  url => binary(),
  http_vsn => atom(),
  method => binary(),
  path => binary(),
  pathInfo => binary(),
  urlQuery => binary()
}.

%%-----------------------------------------------------------------------------
-type auth() :: #{
  user => binary(),
  pass => binary(),
  type => binary()
}.

%%-----------------------------------------------------------------------------
-type host() :: #{
  url => binary(),
  name => binary(),
  port => binary(),
  info => binary()
}.

%%-----------------------------------------------------------------------------
-type peer() :: #{
  ip => tuple(),
  port => integer()
}.



%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @reference The <a href="https://github.com/ninenines/cowboy">Cowboy</a>
%%% github repo.
%%% @see erts_apps
%%%
%%% @doc
%%% The {@module} module provides some Plexo specific helper functions relating
%%% to the Cowboy webserver and libraries.
%%%
%%% ==== Notes ====
%%%
%%% <ol>
%%%   <li>
%%%     The current implementation uses the
%%%     <a href="https://github.com/ninenines/cowboy">Cowboy</a> http server
%%%     libraries.
%%%   </li>
%%% </ol>
%%% @end
%%%----------------------------------------------------------------------------
-module(cb_util).
-author("Temple").

%%%============================================================================
%% Public API
%%%============================================================================

-export([
  is_authorized/2,             % Default authorization method.
  build_rest_action/1,         % Build a map of the HTTP paramters.
  build_auth/1,                % Build a auth map from the HTTP auth header.
  build_action/1,              % Build a map of the HTTP request paramters.
  build_host/1,                % Build a map of the HTTP host data.
  build_peer/1                 % Build a map of the HTTP peer data.
]).

%%%============================================================================
%% Public Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% A centralized authorization method for Cowboy HTTP/REST Handlers.
%%
%% This method currently builds a RestAction entity (that is a map of relevant
%% HTTP Request parameters extracted from the Cowboy Req object), and passes it
%% to the {@link core_auth:restful_auth/1} for authentication and
%% authorization.
%% @end
%%-----------------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), State :: any())
      ->  {{true, Req :: cowboy_req:req(), User :: {binary(),binary()}},
            Req :: cowboy_req:req(), State :: any()}
      |   {{false, Realm :: binary()}, Req :: cowboy_req:req(), State :: any()}.

is_authorized(Req, State) ->
  RestAction = build_rest_action(Req),
  case core_auth:restful_auth(RestAction) of
    true ->
      #{auth := #{user := User, pass :=  Passwd}} = RestAction,
      {true, Req, {User, Passwd}};
    _ ->
      {{false, core_auth:http_basic_realm_hdr(RestAction)}, Req, State}
  end.

%%-----------------------------------------------------------------------------
%% @doc
%% Build a map of selected HTTP parameters associated with this HTTP request.
%%
%% The results can be used for a vareity of purposes such as authentication,
%% fine-grained authorisation, auditing, etc.
%%
%% ==== Example Ouput ====
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
-spec build_rest_action(Req :: cowboy_req:req())
      -> Built :: core_rest:rest_action().

build_rest_action(Req) ->
  RestAction = #{
    auth => build_auth(Req),
    action => build_action(Req),
    host => build_host(Req),
    peer => build_peer(Req)
  },
  lager:debug("Built RestAction: ~p~n", [RestAction]),
  RestAction.

%%-----------------------------------------------------------------------------
%% @doc
%% Build a map of selected HTTP 'authentication header' parameters associated
%% with this HTTP request.
%%
%% The results can be used for a vareity of purposes such as authentication,
%% fine-grained authorisation, auditing, etc.
%%
%% NB: If no credentials are specified then the result is undefined.
%%
%% ==== Example Ouput ====
%%   ```
%%   #{
%%     user => <<"Temple">>,
%%     passwd => <<"Wibble2Wobble">>,
%%     type => <<"basic">>
%%   }
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec build_auth(Req :: cowboy_req:req())
      -> core_rest:auth() | undefined.

build_auth(Req) ->
  case cowboy_req:parse_header(<<"authorization">>, Req) of
    % e.g. {<<"basic">>, {User = <<"Temple">>, <<"Wibble2Wobble">>}}
    {AuthType, {UserIdent, Passwd}} ->
      #{user => UserIdent, pass => Passwd, type => AuthType};
    _ ->
      undefined
  end.

%%-----------------------------------------------------------------------------
%% @doc
%% Build a map of selected HTTP request parameters associated with this HTTP
%% request.
%%
%% The results can be used for a vareity of purposes such as authentication,
%% fine-grained authorisation, auditing, etc.
%%
%% ==== Example Ouput ====
%%   ```
%%   #{
%%     version => 'HTTP/1.1'
%%     url => <<"http://localhost:8877/api/app/sasl">>,
%%     method => <<"PUT">>,
%%     path => <<"/api/app/sasl">>,
%%     pathInfo => undefined,
%%     urlQuery => <<>>,
%%   }
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec build_action(Req :: cowboy_req:req()) -> core_rest:action().

build_action(Req) ->
  #{
    % -> cowboy:http_version()
    version => cowboy_req:version(Req),
    % -> undefined | binary()
    url => cowboy_req:url(Req),
    % -> binary()
    method => cowboy_req:method(Req),
    % -> binary()
    path => cowboy_req:path(Req),
    % -> cowboy_router:tokens() | undefined
    pathInfo => cowboy_req:path_info(Req),
    % -> binary()
    urlQuery => cowboy_req:qs(Req)
  }.

%%-----------------------------------------------------------------------------
%% @doc
%% Build a map of selected HTTP 'host' parameters associated with this HTTP
%% request.
%%
%% The results can be used for a vareity of purposes such as authentication,
%% fine-grained authorisation, auditing, etc.
%%
%% ==== Example Ouput ====
%%   ```
%%   #{
%%     url => <<"http://localhost:8877">>,
%%     name => <<"localhost">>,
%%     port => 8877,
%%     info => undefined,
%%   }
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec build_host(Req :: cowboy_req:req())
      -> core_rest:host().

build_host(Req) ->
  #{
    % -> undefined | binary()
    url => cowboy_req:host_url(Req),
    % -> binary()
    name => cowboy_req:host(Req),
    % -> inet:port_number()
    port => cowboy_req:port(Req),
    % -> cowboy_router:tokens() | undefined
    info => cowboy_req:host_info(Req)
  }.

%%-----------------------------------------------------------------------------
%% @doc
%% Build a map of selected HTTP 'peer' parameters associated with this HTTP
%% request.
%%
%% The results can be used for a vareity of purposes such as authentication,
%% fine-grained authorisation, auditing, etc.
%%
%% ==== Example Ouput ====
%%   ```
%%   #{
%%     ip => {127,0,0,1},
%%     port => 51591
%%   }
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec build_peer(Req :: cowboy_req:req())
      -> core_rest:peer() | undefined.

build_peer(Req) ->
  case cowboy_req:peer(Req) of
  % e.g. {inet:ip_address(), inet:port_number()}
    {IPAddress, Port} ->
      #{ip => IPAddress, port => Port};
    _ ->
      undefined
  end.


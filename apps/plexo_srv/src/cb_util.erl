%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @reference The <a href="https://github.com/ninenines/cowboy">Cowboy</a>
%%% github repo.
%%% @see erts_apps
%%%
%%% @doc
%%% Some helper functions for the Cowboy webserver and libraries.
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
  build_request_nfo/1,         % Build a map of the HTTP request paramters.
  build_auth_nfo/1,            % Build a auth map from the HTTP auth header.
  build_host_nfo/1,            % Build a map of the HTTP host data.
  build_peer_nfo/1             % Build a map of the HTTP peer data.
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
  #{request := #{auth := #{user := User, passwd :=  Passwd}}} = RestAction,
  case core_auth:restful_auth(RestAction) of
    true ->
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
%%-----------------------------------------------------------------------------
-spec build_rest_action(Req :: cowboy_req:req()) -> Built :: core_auth:rest_action().

build_rest_action(Req) ->
  RestAction = #{
    request => build_request_nfo(Req),
    host => build_host_nfo(Req),
    peer => build_peer_nfo(Req)
  },
  io:format("Built RestAction: ~p~n", [RestAction]),
  RestAction.

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
%%     auth => #{
%%       passwd => <<"Wibble2Wobble">>,
%%       type => <<"basic">>,
%%       user => <<"Temple">>
%%     },
%%     method => <<"PUT">>,
%%     path => <<"/api/app/sasl">>,
%%     pathInfo => undefined,
%%     queryString => <<>>,
%%     url => <<"http://localhost:8877/api/app/sasl">>,
%%     version => 'HTTP/1.1'
%%   }
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec build_request_nfo(Req :: cowboy_req:req()) -> core_auth:request_nfo().

build_request_nfo(Req) ->
  #{
    % -> undefined | binary()
    url => cowboy_req:url(Req),
    % -> binary()
    method => cowboy_req:method(Req),
    % -> cowboy:http_version()
    version => cowboy_req:version(Req),
    % -> map()
    auth => build_auth_nfo(Req),
    % -> binary()
    path => cowboy_req:path(Req),
    % -> cowboy_router:tokens() | undefined
    pathInfo => cowboy_req:path_info(Req),
    % -> binary()
    queryString => cowboy_req:qs(Req)
  }.

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
%%     passwd => <<"Wibble2Wobble">>,
%%     type => <<"basic">>,
%%     user => <<"Temple">>
%%   }
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec build_auth_nfo(Req :: cowboy_req:req())
      -> core_auth:auth_nfo() | undefined.

build_auth_nfo(Req) ->
  case cowboy_req:parse_header(<<"authorization">>, Req) of
    % e.g. {<<"basic">>, {User = <<"Temple">>, <<"Wibble2Wobble">>}}
    {AuthType, {UserIdent, Passwd}} ->
      #{type => AuthType, user => UserIdent, passwd => Passwd};
    _ ->
      undefined
  end.

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
%%     info => undefined,
%%     name => <<"localhost">>,
%%     port => 8877,
%%     url => <<"http://localhost:8877">>
%%   }
%%   '''
%% @end
%%-----------------------------------------------------------------------------
-spec build_host_nfo(Req :: cowboy_req:req()) -> core_auth:host_nfo().

build_host_nfo(Req) ->
  #{
    % -> binary()
    name => cowboy_req:host(Req),
    % -> cowboy_router:tokens() | undefined
    info => cowboy_req:host_info(Req),
    % -> inet:port_number()
    port => cowboy_req:port(Req),
    % -> undefined | binary()
    url => cowboy_req:host_url(Req)
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
-spec build_peer_nfo(Req :: cowboy_req:req())
      -> core_auth:peer_nfo() | undefined.

build_peer_nfo(Req) ->
  case cowboy_req:peer(Req) of
  % e.g. {inet:ip_address(), inet:port_number()}
    {IPAddress, Port} ->
      #{ip => IPAddress, port => Port};
    _ ->
      undefined
  end.


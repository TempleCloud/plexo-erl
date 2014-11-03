%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @reference The <a href="https://github.com/ninenines/cowboy">Cowboy</a>
%%% github repo.
%%%
%%% @see erts_apps
%%%
%%% @doc
%%% A Cowboy REST handler that handles operations relating to individual
%%% ERTS 'app' resources.
%%%
%%% When Erlang/OTP applications are considered as RESTful resources, the
%%% REST verbs are mapped as follows:
%%% ```
%%% GET    : Get the configuration details of a specified application.
%%% PUT    : Start the specified application.
%%% DELETE : Stop the specified application.
%%% '''
%%% ==== Notes ====
%%%
%%% <ol>
%%%   <li>
%%%     The handler exposes the {@link erts_apps} module.
%%%   </li>
%%%   <li>
%%%     The current implementation uses the
%%%     <a href="https://github.com/ninenines/cowboy">Cowboy</a> http server.
%%%     libraries.
%%%   </li>
%%% </ol>
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(cb_app_hndlr).
-author("Temple").

%%%============================================================================
%% Cowboy Handler Callback API
%%%============================================================================

% Cowboy API Callbacks
-export([
  init/2,                       % Initialise a new request handling process.
  is_authorized/2,              % Check authorization.
  allowed_methods/2,            % Define allowed HTTP methods.
  content_types_provided/2,     % Defines provided types and handling.
  content_types_accepted/2,     % Defines accepted types and handling.
  delete_resource/2,            % Handle delete an resource.
  terminate/3                   % Clean up after a request has been processed.
]).

% Custom Handler Callbacks
-export([
  handle_provide_as_json/2,     % Handle resource provision as json.
  handle_accept_from_url/2,     % Handle new resource from uri params.
  handle_delete_from_url/2      % Handle resource deletions from uri params.
]).

% Support Functions
-export([
  get_app_cnfg/2,               % Get the app configuration as json.
  start_app/2,                  % Start the app and send result as json.
  stop_app/2                    % Stop the app and send result as json.
]).

%%%============================================================================
%%% Cowboy Handler Callback API - Implementation
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Initialise a new Cowboy REST request handler process.
%% @end
%%-----------------------------------------------------------------------------
-spec init(Req :: cowboy_req:req(), Opts :: any())
      -> {'cowboy_rest', Req :: cowboy_req:req(), Opts :: any()}.

init(Req, Opts) ->
  io:format("Init Handler: ~p ~n", [Req]),
  {cowboy_rest, Req, Opts}.

%%-----------------------------------------------------------------------------
%% @doc
%% Delegate the Cowboy Req to the appropriate handling function based on
%% content_type.
%%
%% What authentication mechanisms should I provide? For example: form-based,
%% token-based (in the URL or a cookie), HTTP basic, HTTP digest, SSL certificate
%% or any other form of authentication
%% @end
%%-----------------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), State :: any())
      -> {{true, Req :: cowboy_req:req(), User :: {binary(),binary()}},
      Req :: cowboy_req:req(), State :: any()}
    | {{false, Realm :: binary()}, Req :: cowboy_req:req(), State :: any()}.

is_authorized(Req, State) ->
  AuthHeader = cowboy_req:parse_header(<<"authorization">>, Req),
  io:format("AuthHeader ~p~n", [AuthHeader]),
  % {<<"basic">>, {User, Passwd}} = AuthHeader,
  % io:format("Username: ~p, Password: ~p~n", [User, Passwd]),
  case AuthHeader of
    {<<"basic">>, {User = <<"Temple">>, <<"Wibble2Wobble">>}} ->
      {true, Req, User};
    _ ->
      {{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
  end.

%%-----------------------------------------------------------------------------
%% @doc
%% Allow OPTIONS, GET (get app details), PUT (start app), DELETE (stop app).
%% All responses are retuned as JSON.
%% @end
%%-----------------------------------------------------------------------------
-spec allowed_methods(Req :: cowboy_req:req(), State :: any())
      -> {Allowed :: list(), Req :: cowboy_req:req(), State :: any()}.

allowed_methods(Req, State) ->
  Allowed = [<<"OPTIONS">>, <<"GET">>, <<"PUT">>, <<"DELETE">>],
  {Allowed, Req, State}.

%%-----------------------------------------------------------------------------
%% @doc
%% Delegate the Cowboy Req to the appropriate handling function based on
%% content_type.
%%
%% Handles "application/json".
%% @end
%%-----------------------------------------------------------------------------
-spec content_types_provided(Req :: cowboy_req:req(), State :: any())
      -> {Handlers :: list(), Req :: cowboy_req:req(), State :: any()}.

content_types_provided(Req, State) ->
  Provided = <<"application/json">>,
  {[
    {Provided, handle_provide_as_json}
  ], Req, State}.

%%-----------------------------------------------------------------------------
%% @doc
%% Accepted form parameters are uri-template-encoded.
%% ```.../app/{app_name}'''.
%% @end
%%-----------------------------------------------------------------------------
-spec content_types_accepted(Req :: cowboy_req:req(), State :: any())
      -> {Handlers :: list(), Req :: cowboy_req:req(), State :: any()}.

content_types_accepted(Req, State) ->
  Accepted = {<<"application">>, <<"x-www-form-urlencoded">>, []},
  {[{
    Accepted, handle_accept_from_url
    }],
  Req, State}.

%%-----------------------------------------------------------------------------
%% @doc
%% DELETE operations 'stop' the specified app.
%% @end
%%-----------------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: any())
      -> {true, Req :: cowboy_req:req(), State :: any()}.

delete_resource(Req, State) ->
  handle_delete_from_url(Req, State).

%%-----------------------------------------------------------------------------
%% @doc
%% Handle terminating a Cowboy REST request handler process.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(any(), _Req :: cowboy_req:req(), _State :: any())
      -> ok when _Req::cowboy_req:req().

terminate(Reason, _Req, _State) ->
  io:format("terminate: ~p ~n", [Reason]),
  ok.


%%=============================================================================
%% Custom Callback API
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Handle GET requests for providing the details of the specified application
%% resource; and writing the representation back to the client as JSON.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_provide_as_json(Req :: cowboy_req:req(), State :: any())
      -> {Json :: binary(), Req :: cowboy_req:req(), State :: any()}.

handle_provide_as_json(Req, State) ->
  case cowboy_req:method(Req) of
    <<"GET">> ->
      get_app_cnfg(Req, State);
    _ ->
      JsonRS = core_json:to_json(#{<<"result">> => <<"{Error}">>}),
      {JsonRS, Req, State}
  end.

%%-----------------------------------------------------------------------------
%% @doc
%% Handle PUT requests for 'starting' (creating a new instance) the specified
%% application resource; and writing the outcome of the operation back to the
%% requesting client as JSON.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_accept_from_url(Req :: cowboy_req:req(), State :: any())
      -> {true, Req :: cowboy_req:req(), State :: any()}.

handle_accept_from_url(Req, State) ->
  case cowboy_req:method(Req) of
    <<"PUT">> ->
      start_app(Req, State);
    _ ->
      {true, Req, State}
  end.

%%-----------------------------------------------------------------------------
%% @doc
%% Handle DELETE requests for 'stopping' the specified application resource; and
%% writing the outcome of the operation back to the requesting client as JSON.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_delete_from_url(Req :: cowboy_req:req(), State :: any())
      -> {true, Req :: cowboy_req:req(), State :: any()}.

handle_delete_from_url(Req, State) ->
  case cowboy_req:method(Req) of
    <<"DELETE">> ->
      stop_app(Req, State);
    _ ->
      {true, Req, State}
  end.

%%=============================================================================
%% Support Functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Get the specified Erlang 'application' details and return the result to the
%% client.
%% @end
%%-----------------------------------------------------------------------------
-spec get_app_cnfg(Req :: cowboy_req:req(), State :: any())
      -> {Json :: binary(), Req :: cowboy_req:req(), State :: any()}.

get_app_cnfg(Req, State) ->
  App = extract_uri_atom(app_name, Req),
  ResMap = erts_apps:get_app_cnfg(App),
  Json = core_json:to_json(ResMap),
  {Json, Req, State}.

%%-----------------------------------------------------------------------------
%% @doc
%% Start the specified Erlang 'application' and return the result to the
%% client.
%% @end
%%-----------------------------------------------------------------------------
-spec start_app(Req :: cowboy_req:req(), State :: any())
      -> {true, Req2 :: cowboy_req:req(), State :: any()}.

start_app(Req, State) ->
  App = extract_uri_atom(app_name, Req),
  Res = erts_apps:start_apps(App),
  Req2 = send_app_status_response(Res, Req),
  {true, Req2, State}.

%%-----------------------------------------------------------------------------
%% @doc
%% Stop the specified Erlang 'application' and return the result to the
%% client.
%% @end
%%-----------------------------------------------------------------------------
-spec stop_app(Req :: cowboy_req:req(), State :: any())
      -> {true, Req :: cowboy_req:req(), State :: any()}.

stop_app(Req, State) ->
  App = extract_uri_atom(app_name, Req),
  Res = erts_apps:stop_apps(App),
  Req2 = send_app_status_response(Res, Req),
  {true, Req2, State}.


%%=============================================================================
%% Private Functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Extract the specified cowboy request binding and convert it to an atom.
%% @end
%%-----------------------------------------------------------------------------
-spec extract_uri_atom(Binding :: atom(), Req :: cowboy_req:req()) -> atom().

extract_uri_atom(Binding, Req) ->
  Bound = cowboy_req:binding(Binding, Req),
  binary_to_atom(Bound, utf8).

%%-----------------------------------------------------------------------------
%% @doc
%% Build a JSON 'app status' response and send it to the client.
%% @end
%%-----------------------------------------------------------------------------
-spec send_app_status_response(Res :: {_,_}, Req :: cowboy_req:req())
       -> cowboy_req:req().

send_app_status_response({AppName, AppStatus}, Req) ->
  Res = #{<<"appName">> => AppName, <<"appStatus">> => AppStatus},
  Json = core_json:to_json(Res),
  Req2 = cowboy_req:set_resp_body(Json, Req),
  Req2.

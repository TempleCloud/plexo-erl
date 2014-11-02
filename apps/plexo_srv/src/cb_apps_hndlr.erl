%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @reference The <a href="https://github.com/ninenines/cowboy">Cowboy</a>
%%% github repo.
%%% @see erts_apps
%%%
%%% @doc
%%% A Cowboy REST handler that handles operations relating to a collection of
%%% ERTS 'app' resources.
%%%
%%% When Erlang/OTP applications are considered as RESTful resources, the
%%% REST verbs associated with the collection are mapped as follows:
%%% ```
%%% GET    : Get the configuration details of a specified application.
%%% POST   : Start the specified application.
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
%%% @end
%%%----------------------------------------------------------------------------
-module(cb_apps_hndlr).
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
  terminate/3                   % Clean up after a request has been processed.
]).

% Custom Handler Callbacks
-export([
  handle_provide_as_json/2,     % Handle resource provision as json.
  handle_accept_from_url/2      % Handle new resource from uri params.
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
  {<<"basic">>, {User, Passwd}} = AuthHeader,
  io:format("Username: ~p, Password: ~p~n", [User, Passwd]),

  case AuthHeader of
    {<<"basic">>, {User = <<"Temple">>, <<"Wibble2Wobble">>}} ->
      {true, Req, User};
    _ ->
      {{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
  end.

%%-----------------------------------------------------------------------------
%% @doc
%% Allow OPTIONS, GET (get app details), and POST (start app).
%% All responses are retuned as JSON.
%% @end
%%-----------------------------------------------------------------------------
-spec allowed_methods(Req :: cowboy_req:req(), State :: any())
      -> {Allowed :: list(), Req :: cowboy_req:req(), State :: any()}.

allowed_methods(Req, State) ->
  {
    [<<"OPTIONS">>, <<"GET">>, <<"POST">>],
    Req, State}.

%%-----------------------------------------------------------------------------
%% @doc
%% Delegate the Cowboy Req to the appropriate handling function based on
%% content_type.
%%
%% Handles "application/json".
%% @end
%%-----------------------------------------------------------------------------
-spec content_types_provided(Req :: cowboy_req:req(), State :: any())
      -> {[{binary(), atom()},...], Req :: cowboy_req:req(), State :: any()}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, handle_provide_as_json}
    ], Req, State}.

%%-----------------------------------------------------------------------------
%% @doc
%% Accepted form parameters are uri-template-encoded.
%% ```.../apps/{app_name}'''.
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
%% Handle GET requests for providing a list of application resources on the
%% node.
%%
%% The 'status' uri query parameter can be used to specify 'loaded' or 'running'
%% apps. The default is 'running'.
%%
%% The result is written back to the client as JSON.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_provide_as_json(Req :: cowboy_req:req(), State :: any())
      -> {JsonRS :: binary(), Req :: cowboy_req:req(), State :: any()}.

handle_provide_as_json(Req, State) ->
  case maps:get(status, cowboy_req:match_qs([status], Req)) of
    <<"loaded">> ->
      get_loaded_apps_as_json(Req, State);
    <<"running">> ->
      get_running_apps_as_json(Req, State);
    _ ->
      JsonRS = core_json:to_json(#{<<"result">> => <<"{Error}">>}),
      {JsonRS, Req, State}
  end.

%%-----------------------------------------------------------------------------
%% @doc
%% Handle PUT requests for starting (creating a new instance of) the specified
%% application resource; and writing the outcome of the operation back to the
%% requesting client as JSON.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_accept_from_url(Req :: cowboy_req:req(), State :: any())
      -> {true, Req :: cowboy_req:req(), State :: any()}.

handle_accept_from_url(Req, State) ->
  case cowboy_req:method(Req) of
    <<"POST">> ->
      cb_app_hndlr:start_app(Req, State);
    _ ->
      {true, Req, State}
  end.


%%=============================================================================
%% Private Functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Get the ERTS 'loaded apps' as a binary JSON type.
%% @end
%%-----------------------------------------------------------------------------
-spec get_loaded_apps_as_json(Req :: cowboy_req:req(), State :: any())
      -> {Json :: binary(), Req :: cowboy_req:req(), State :: any()}.

get_loaded_apps_as_json(Req, State) ->
  Loaded = erts_apps:get_loaded_apps(),
  Json = core_json:to_json(Loaded),
  {Json, Req, State}.

%%-----------------------------------------------------------------------------
%% @doc
%% Get the ERTS 'running apps' as a binary JSON type.
%% @end
%%-----------------------------------------------------------------------------
-spec get_running_apps_as_json(Req :: cowboy_req:req(), State :: any())
      -> {Json :: binary(), Req :: cowboy_req:req(), State :: any()}.

get_running_apps_as_json(Req, State) ->
  Running = erts_apps:get_running_apps(),
  Json = core_json:to_json(Running),
  {Json, Req, State}.


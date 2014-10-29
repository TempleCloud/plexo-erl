%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @reference The <a href="https://github.com/ninenines/cowboy">Cowboy</a>
%%% github repo.
%%% @see erts_apps
%%%
%%% @doc
%%% A Cowboy REST handler that handles operations relating to 'apps' in
%%% ERTS.
%%%
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
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2,     % Get the 'apps'.
  terminate/3                   % Clean up after a request has been processed.
]).

-export([
  handle_get_apps_as_json/2,    % Get the require app froup as a JSON type.
  get_loaded_apps_as_json/2,    % Get the 'loaded apps' apps as a JSON type.
  get_running_apps_as_json/2,   % Get the 'running apps' apps as a JSON type.
  start_app/2
]).


%%%============================================================================
%%% Cowboy Handler Callback API - Implementation
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Initialise a Cowboy REST request handler process.
%% @end
%%-----------------------------------------------------------------------------
-spec init(Req :: cowboy_req:req(), Opts :: any())
      -> {'cowboy_rest', Req :: cowboy_req:req(), Opts :: any()}.
init(Req, Opts) ->
  io:format("Init Handler: ~p ~n", [Req]),
  {cowboy_rest, Req, Opts}.


allowed_methods(Req, State) ->
  {
    [<<"GET">>, <<"POST">>],
    Req, State}.



%%-----------------------------------------------------------------------------
%% @doc
%% Delegate the Cowboy Req to the appropriate handling function based on
%% content_type.
%% @end
%%-----------------------------------------------------------------------------
-spec content_types_provided(Req :: cowboy_req:req(), State :: any())
      -> {[{binary(), atom()},...], Req :: cowboy_req:req(), State :: any()}.
content_types_provided(Req, State) ->
  io:format("content_types_provided!~n"),
  {[
    {<<"application/json">>, handle_get_apps_as_json}
    ], Req, State}.


content_types_accepted(Req, State) ->
  io:format("content_types_accepted!~n"),
  {
    [{{
      <<"application">>,
      <<"x-www-form-urlencoded">>,
      []
    }, start_app}],
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

start_app(Req, State) ->
  io:format("start_app... ~n"),
  AppName = cowboy_req:binding(name, Req),
  io:format("start_app: ~p ~n", [AppName]),
  App = binary_to_atom(AppName, utf8),
  io:format("start_app: ~p ~n", [App]),
  Res = erts_apps:start_app(App),
  io:format("start_app: ~p ~n", [Res]),
  % Json = core_json:to_json(Res),
  % io:format("start_app: ~p ~n", [Json]),
  % Req2 = cowboy_req:set_resp_body(Json, Req),
  Req2 = cowboy_req:set_resp_body(AppName, Req),
  {true, Req2, State}.


%%-----------------------------------------------------------------------------
%% @doc
%% Get the ERTS 'loaded apps' as a binary JSON type, and return it as the
%% 'body' of the handling Cowboy response 3-tuple.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_get_apps_as_json(Req :: cowboy_req:req(), State :: any())
      -> {JsonRS :: binary(), Req :: cowboy_req:req(), State :: any()}.
handle_get_apps_as_json(Req, State) ->
  io:format("handle_get_apps_as_json... ~n"),
  case cowboy_req:binding(state, Req) of
    <<"loaded">> ->
      get_running_apps_as_json(Req, State);
    <<"running">> ->
      get_loaded_apps_as_json(Req, State);
    _ ->
      {<<"Error">>, Req, State}
  end.


%%-----------------------------------------------------------------------------
%% @doc
%% Get the ERTS 'loaded apps' as a binary JSON type, and return it as the
%% 'body' of the handling Cowboy response 3-tuple.
%% @end
%%-----------------------------------------------------------------------------
-spec get_loaded_apps_as_json(Req :: cowboy_req:req(), State :: any())
      -> {JsonRS :: binary(), Req :: cowboy_req:req(), State :: any()}.
get_loaded_apps_as_json(Req, State) ->
  io:format("Handling Request: ~p ~n", [Req]),
  LoadedApps = erts_apps:get_loaded_apps(),
  JsonRS = core_json:to_json(LoadedApps),
  io:format("Handled Request. ~n"),
  {JsonRS, Req, State}.


%%-----------------------------------------------------------------------------
%% @doc
%% Get the ERTS 'loaded apps' as a binary JSON type, and return it as the
%% 'body' of the handling Cowboy response 3-tuple.
%% @end
%%-----------------------------------------------------------------------------
-spec get_running_apps_as_json(Req :: cowboy_req:req(), State :: any())
      -> {JsonRS :: binary(), Req :: cowboy_req:req(), State :: any()}.
get_running_apps_as_json(Req, State) ->
  io:format("Handling Request: ~p ~n", [Req]),
  RunningApps = erts_apps:get_running_apps(),
  JsonRS = core_json:to_json(RunningApps),
  io:format("Handled Request. ~n"),
  {JsonRS, Req, State}.






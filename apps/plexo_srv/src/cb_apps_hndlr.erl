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
  content_types_provided/2,     % Get the 'loaded apps'.
  terminate/3                   % Clean up after a request has been processed.
]).

-export([
  handle_apps_as_json/2,        % Get the require app froup as a JSON type.
  get_loaded_apps_as_json/2,    % Get the 'loaded apps' apps as a JSON type.
  get_running_apps_as_json/2    % Get the 'running apps' apps as a JSON type.
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


%%-----------------------------------------------------------------------------
%% @doc
%% Delegate the Cowboy Req to the appropriate handling function based on
%% content_type.
%% @end
%%-----------------------------------------------------------------------------
-spec content_types_provided(Req :: cowboy_req:req(), State :: any())
      -> {[{binary(), atom()},...], Req :: cowboy_req:req(), State :: any()}.
content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, handle_apps_as_json}
    ], Req, State}.


%%-----------------------------------------------------------------------------
%% @doc
%% Handle terminating a Cowboy REST request handler process.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(any(), _Req :: cowboy_req:req(), _State :: any())
      -> ok when _Req::cowboy_req:req().
terminate(Reason, _Req, _State) ->
  io:format("Terminate Handler: ~p ~n", [Reason]),
  ok.



%%-----------------------------------------------------------------------------
%% @doc
%% Get the ERTS 'loaded apps' as a binary JSON type, and return it as the
%% 'body' of the handling Cowboy response 3-tuple.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_apps_as_json(Req :: cowboy_req:req(), State :: any())
      -> {JsonRS :: binary(), Req :: cowboy_req:req(), State :: any()}.
handle_apps_as_json(Req, State) ->

  case cowboy_req:binding(state, Req) of
    <<"loaded">> ->
      get_running_apps_as_json(Req, State);
    <<"running">> ->
      get_loaded_apps_as_json(Req, State);
    _ ->
      {undefined, Req, State}
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






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
-module(cb_app_hndlr).
-author("Temple").

%%%============================================================================
%% Cowboy Handler Callback API
%%%============================================================================

% Cowboy API Callbacks
-export([
  init/2,                       % Initialise a new request handling process.
  content_types_provided/2,
  allowed_methods/2,
  delete_resource/2,
  terminate/3                   % Clean up after a request has been processed.
]).

-export([
  stop_app/2,
  get_app_cnfg/2
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


content_types_provided(Req, State) ->
  io:format("content_types_provided!~n"),
  {[
    {<<"application/json">>, get_app_cnfg}
  ], Req, State}.



allowed_methods(Req, State) ->
  {
    [<<"OPTIONS">>, <<"GET">>, <<"DELETE">>],
    Req, State}.


delete_resource(Req, State) ->
  io:format("delete_resource"),
  stop_app(Req, State).


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


  % application:get_all_key(plexo_srv).

%%=============================================================================
%% Custom Callback API
%%=============================================================================

stop_app(Req, State) ->
  io:format("stop_app... ~n"),
  AppParam = cowboy_req:binding(app_name, Req),
  App = binary_to_atom(AppParam, utf8),
  {AppName, AppState} = erts_apps:stop_apps(App),
  Res = #{<<"appName">> => AppName, <<"state">> => AppState},
  Json = core_json:to_json(Res),
  Req2 = cowboy_req:set_resp_body(Json, Req),
  {true, Req2, State}.


%%-----------------------------------------------------------------------------
%% @doc
%% Get the ERTS 'loaded/running apps' as a binary JSON type, and return it as
%% the 'body' of the handling Cowboy response 3-tuple.
%% @end
%%-----------------------------------------------------------------------------
-spec get_app_cnfg(Req :: cowboy_req:req(), State :: any())
      -> {JsonRS :: binary(), Req :: cowboy_req:req(), State :: any()}.
get_app_cnfg(Req, State) ->
  io:format("get_app_cnf_as_json... ~n"),
  AppParam = cowboy_req:binding(app_name, Req),
  App = binary_to_atom(AppParam, utf8),
  ResMap = erts_apps:get_app_cnfg(App),
  Json = core_json:to_json(ResMap),
  io:format("get_app_cnf_as_json...~p~n", [Json]),
  {Json, Req, State}.


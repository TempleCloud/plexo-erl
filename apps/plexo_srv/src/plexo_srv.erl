%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% A helper class to start the 'plexo_srv' server application.
%%% @end
%%%----------------------------------------------------------------------------
-module(plexo_srv).
-author("Temple").

%%% Export all declared functions when TEST.
-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
  -compile(export_all).
-endif.

%%%============================================================================
%%% Public API
%%%============================================================================

-export([
	start/0,                     % Start the plexo_srv app.
  stop/0                       % Stop the plexo_srv app.
]).

%%%============================================================================
%%% Public Functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Start the required 'plexo_serv' applications.
%% @end
%%-----------------------------------------------------------------------------
-spec start()
      -> Res :: erts_apps:app_start_res() | list(erts_apps:app_start_res()).

start() ->
  Apps = [crypto, ranch, cowlib, cowboy, plexo_srv],
  Res = erts_apps:start_apps(Apps),
  io:format("Res: ~p~n", [Res]),
  Res.

%%-----------------------------------------------------------------------------
%% @doc
%% Stop the required 'plexo_serv' applications.
%% @end
%%-----------------------------------------------------------------------------
-spec stop()
      -> Res :: erts_apps:app_stop_res() | list(erts_apps:app_stop_res()).

stop() ->
  Apps = [crypto, ranch, cowlib, cowboy, plexo_srv],
  Res = erts_apps:stop_apps(Apps),
  io:format("Res: ~p~n", [Res]),
  Res.


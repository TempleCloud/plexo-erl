%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% The {@module} OTP 'application behaviour' for the Plexomancer http server.
%%% @end
%%%----------------------------------------------------------------------------
-module(plexo_srv_app).

%% ============================================================================
%% OTP Application Behaviour
%% ============================================================================

-behaviour(application).
-export([
	start/2,                     % Start the plexo_srv application.
	stop/1                       % Stop the plexo_srv application.
]).

%% ============================================================================
%% OTP Application Behaviour Callback Implementation
%% ============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Start the Plexo server.
%%
%% lsof -n | grep LISTEN
%%
%% @end
%%-----------------------------------------------------------------------------
-spec start(_,_) -> {'ok', pid()}.

start(_Type, _Args) ->

	Port = 8877,
	% Port = 8080,
  Num_Sockets = 100,

	io:format("Starting Cowboy on port ~p...~n", [Port]),

	io:format("RootDir ~p...~n", [code:root_dir()]),
	io:format("LibDir ~p...~n", [code:lib_dir()]),
	io:format("AppDir ~p...~n", [code:lib_dir(plexo_srv)]),
	io:format("AppPrivDir ~p...~n", [code:priv_dir(plexo_srv)]),

	AnyHostRoutes = {'_', [
 		{"/", cowboy_static,
      {priv_file, plexo_srv, "assets/plexo.html"}},
 		{"/assets/controllers/[...]", cowboy_static,
      {priv_dir, plexo_srv, "assets/controllers"}
    },
    % GET ?status=(loaded|running)
    {"/api/apps", cb_apps_hndlr, []},
    % POST
    {"/api/apps/:app_name", cb_apps_hndlr, []},
    % GET / PUT / DELETE
    {"/api/app/:app_name", cb_app_hndlr, []}

	]},

	Routes = [AnyHostRoutes],

	Dispatch = cowboy_router:compile(Routes),

	{ok, _} = cowboy:start_http(http, Num_Sockets, [{port, Port}], [
		{env, [{dispatch, Dispatch}]}
	]),

  io:format("Started Server...~n"),

	plexo_srv_sup:start_link().

%%-----------------------------------------------------------------------------
%% @doc
%% Stop the Plexo server.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(_) -> 'ok'.

stop(_State) ->
	ok.


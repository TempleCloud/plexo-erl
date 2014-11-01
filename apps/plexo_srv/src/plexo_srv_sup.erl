%%%----------------------------------------------------------------------------
%%% @author Temple
%%%
%%% @doc
%%% The {@module} OTP 'superviser behaviour' for the Plexomancer http server.
%%% @end
%%%----------------------------------------------------------------------------
-module(plexo_srv_sup).

%%%============================================================================
%%% OTP Supervisor Behaviour
%%%============================================================================

-behaviour(supervisor).

-export([
	init/1
]).

%%%============================================================================
%% Public API
%%%============================================================================

-export([
	start_link/0
]).

%%%============================================================================
%%% Macro Definitions
%%%============================================================================

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


%%%============================================================================
%%% PlexoServSup API - Implementation
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Start the Plexo server.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%%============================================================================
%%% OTP Supervisor Behaviour - Callback Implementation
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc
%% Initialise asupervised 'plxo_srv' instance.
%% @end
%%-----------------------------------------------------------------------------
-spec init([])
      -> {'ok', {{'one_for_one', non_neg_integer(), non_neg_integer()}, []}}.

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 10, 10}, Procs}}.

%% init([]) ->
%%
%% 	% Define the 'sc_element_sup' supervisor process.
%% 	%
%% 	SCElementSup = {
%% 		sc_element_sup,                     % The Id to identify the child specification.
%% 		{sc_element_sup, start_link, []},   % The apply(M, F, A) tuple to start the process.
%% 		permanent,                          % Child process always restarted.
%% 		2000,                               % Terminate child: 'exit(Child, shutdown)' timeout.
%% 		supervisor,                         % Child is supervisor process.
%% 		[sc_element]                        % The name of the callback module.
%% 	},
%%
%% 	% Define the 'sc_event' event process.
%% 	%
%% 	EventManager = {
%% 		sc_event,                           % The Id to identify the child specification.
%% 		{sc_event, start_link, []},         % The apply(M, F, A) tuple to start the process.
%% 		permanent,                          % Child process always restarted.
%% 		2000,                               % Terminate child: 'exit(Child, shutdown)' timeout.
%% 		worker,                             % Child is supervisor process.
%% 		[sc_event]                          % The name of the callback module.
%% 	},
%%
%% 	Children = [SCElementSup, EventManager],
%%
%% 	% Strategy : 'one_for_one'
%% 	%
%% 	% If a child process terminates, only that process is restarted.
%% 	%
%% 	%
%% 	% Maximum Restart Frequency : (4, 3600)
%% 	%
%% 	% If more than '4' restarts occur in the last '3600' seconds, then the supervisor
%% 	% terminates all the child processes and then itself.
%% 	%
%% 	RestartStrategy = {one_for_one, 4, 3600},
%%
%% 	{ok, {RestartStrategy, Children}}.

%% Feel free to use, reuse and abuse the code in this file.


%% supervisor.




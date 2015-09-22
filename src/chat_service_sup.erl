-module(chat_service_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ServerProcSpec = {
        chat_service_server,
        {chat_service_server, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [chat_service_server]},
    {ok, { {one_for_one, 1000, 1}, [ServerProcSpec]} }.


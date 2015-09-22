-module(chat_service_server).
-author("prots").

-behaviour(gen_server).
-include("chat_service.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    true = register(chat_server, self()),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({connect, ClientPid, ClientNode}, State) ->
    ok = chat_service_cache:save_contact(ClientPid, ClientNode),
    ContactsList = lists:delete({ClientPid, ClientNode}, chat_service_cache:get_connected_clients()),
    {ClientPid, ClientNode} ! {contacts, ContactsList},
    [{Pid, Node} ! {new_connection, {ClientPid, ClientNode}} || {Pid, Node} <- ContactsList],
    {noreply, State};
handle_info({disconnect, ClientPid, ClientNode}, State) ->
    ok = chat_service_cache:drop_contact(ClientPid, ClientNode),
    ContactsList = chat_service_cache:get_connected_clients(),
    [{Pid, Node} ! {disconnected, {ClientPid, ClientNode}} || {Pid, Node} <- ContactsList],
    {noreply, State};
handle_info({get_contacts, ClientPid, ClientNode}, State) ->
    ContactsList = lists:delete({ClientPid, ClientNode}, chat_service_cache:get_connected_clients()),
    {ClientPid, ClientNode} ! {contacts, ContactsList},
    {noreply, State};
handle_info({send_message, {ToPid, ToNode}, {FromPid, FromNode}, Msg}, State) ->
    case chat_service_cache:check_status(ToPid, ToNode) of
        ?CONNECTED -> {ToPid, ToNode} ! {message, {FromPid, FromNode}, Msg};
        ?DISCONNECTED -> ok
    end,
    {noreply, State};
handle_info(Msg, State) ->
    ?LOG_WARNING("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

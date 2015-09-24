-module(chat_client).
-author("prots").

-behaviour(gen_server).

-define(SERVER_NAME, chat_server).
-define(SERVER_NODE, 'chat_service@127.0.0.1').
-define(CLIENT_NAME, node()).

-include("chat_service.hrl").

%% API
-export([start/0]).
-export([start_link/0]).
-export([connect/0, disconnect/0, send_message/2, get_contacts/0]).


%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    start_link().

start_link() ->
    gen_server:start_link(?MODULE, [], []).

connect() ->
    gen_server:call({?SERVER_NAME, ?SERVER_NODE}, jsx:encode([{<<"type">>, <<"connect">>},
        {<<"args">>, [{<<"client_name">>, ?CLIENT_NAME}]}])).

disconnect() ->
    gen_server:cast({?SERVER_NAME, ?SERVER_NODE}, jsx:encode([{<<"type">>, <<"disconnect">>},
        {<<"args">>, [{<<"client_name">>, ?CLIENT_NAME}]}])).

send_message(ToClient, Msg) ->
    gen_server:cast({?SERVER_NAME, ?SERVER_NODE}, jsx:encode([{<<"type">>, <<"send_message">>},
        {<<"args">>, [{<<"from">>, ?CLIENT_NAME}, {<<"to">>, ToClient}, {<<"msg">>, Msg}]}])).

get_contacts() ->
    gen_server:call({?SERVER_NAME, ?SERVER_NODE}, jsx:encode([{<<"type">>, <<"get_contacts">>},
        {<<"args">>, [{<<"client_name">>, ?CLIENT_NAME}]}])).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================


%% TODO Link to the server
init([]) ->
    true = register(node(), self()),
    {ok, #state{}}.

handle_call(Request, _From, State) ->
    io:format("Unexpected call request: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(MsgBin, State) ->
    io:format("Notification: ~p~n", [MsgBin]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


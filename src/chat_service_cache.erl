-module(chat_service_cache).
-author("prots").

-include("chat_service.hrl").

%% API
-export([init/0]).
-export([save_contact/2]).
-export([drop_contact/2]).
-export([get_connected_clients/0]).
-export([check_status/2]).

init() ->
    _ = ets:new(?CONNECTIONS_TABLE, [named_table, public, set,
        {read_concurrency, true}]),
    _ = ets:new(?MESSAGES_TABLE, [named_table, public, set,
        {read_concurrency, true}]),
    ok.

save_contact(Pid, Node) ->
    DateTime = calendar:local_time(),
    true = ets:insert(?CONNECTIONS_TABLE, {{Pid, Node}, DateTime, ?CONNECTED}),
    ok.

drop_contact(Pid, Node) ->
    true = ets:update_element(?CONNECTIONS_TABLE, {Pid, Node}, {3, ?DISCONNECTED}),
    ok.

get_connected_clients() ->
    [Pid || {Pid, _, Status} <- ets:tab2list(?CONNECTIONS_TABLE), Status =:= ?CONNECTED].

check_status(Pid, Node) ->
    case ets:lookup(?CONNECTIONS_TABLE, {Pid, Node}) of
        [] -> ?DISCONNECTED;
        [{{Pid, Node}, _, Status}|_] -> Status
    end.



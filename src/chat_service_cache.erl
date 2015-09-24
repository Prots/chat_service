-module(chat_service_cache).
-author("prots").

-include("chat_service.hrl").

%% API
-export([init/0]).
-export([save_contact/2]).
-export([drop_contact/1]).
-export([get_connected_clients/0]).
-export([check_status/2]).
-export([get_client_address/1]).

%% API
init() ->
    _ = ets:new(?CONNECTIONS_TABLE, [named_table, public, set,
        {read_concurrency, true}]),
    ok.

save_contact(ClientName, ClientAdress) ->
    DateTime = calendar:local_time(),
    true = ets:insert(?CONNECTIONS_TABLE, {ClientName, ClientAdress, DateTime, ?CONNECTED}),
    ok.

drop_contact(Client) ->
    true = ets:update_element(?CONNECTIONS_TABLE, Client, {4, ?DISCONNECTED}),
    ok.

get_connected_clients() ->
    [ClientName || {ClientName, _, _, _} <- ets:match_object(?CONNECTIONS_TABLE, {'_', '_', '_', ?CONNECTED})].

check_status(ClientName, ClientAddr) ->
    case ets:lookup(?CONNECTIONS_TABLE, ClientName) of
        [] -> check_addr(ClientAddr);
        [{ClientName, ClientAddr, _, Status}|_] -> Status;
        [{ClientName, _Addr, _, _Status}|_] -> ?NAME_IN_USE
    end.

get_client_address(ClientName) ->
    case ets:lookup(?CONNECTIONS_TABLE, ClientName) of
        [{ClientName, ClientAddress, _, ?CONNECTED}|_] -> ClientAddress;
        _ -> undefined
    end.

%%INTERNAL
check_addr(Addr) ->
    case ets:match_object(?CONNECTIONS_TABLE, {'_', Addr, '_', ?CONNECTED}) of
        [] -> ?DISCONNECTED;
        _ -> ?ADDR_IN_USE
    end.


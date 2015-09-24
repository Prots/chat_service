-module(chat_service_protocol).
-author("prots").

-include("chat_service.hrl").

%% API
-export([decode/1]).
-export([encode/1]).
-export([process_request/4]).

%% API
decode(<<"\r\n">>) ->
    <<"\r\n">>;
decode(Msg) when is_binary(Msg) ->
    try
        FormatedMsg = binary:replace(Msg, <<"\r\n">>, <<>>),
        jsx:decode(FormatedMsg)
    catch
        _:_  -> []
    end;
decode(Msg) ->
    Msg.

encode(<<>>) ->
    <<>>;
encode(Msg) ->
    jsx:encode(Msg).

process_request(undefined, undefined, _, _) ->
    <<"unexpected_msg">>;
process_request(<<"connect">>, Args, Opts, ClientType) ->
    NewClientName = proplists:get_value(<<"client_name">>, Args),
    NewClientAddr = get_client_addr_from_req(NewClientName, Opts, ClientType),
    case chat_service_cache:check_status(NewClientName, NewClientAddr) of
        ?DISCONNECTED ->
            ok = chat_service_cache:save_contact(NewClientName, NewClientAddr),
            ContactsList = [{<<"client_name">>, ClientName} ||
                ClientName <- chat_service_cache:get_connected_clients(), ClientName =/= NewClientName],
            ok = send_notifications(ContactsList,
                [{<<"type">>, <<"new_connection">>}, {<<"args">>, Args}]),
            [{<<"type">>, <<"contacts">>}, {<<"args">>, ContactsList}];
        ?CONNECTED ->
            <<"already connected">>;
        Status ->
            Status
    end;
process_request(<<"disconnect">>, Args, Opts, ClientType) ->
    OldClientName = proplists:get_value(<<"client_name">>, Args),
    OldClientAddr = get_client_addr_from_req(OldClientName, Opts, ClientType),
    case chat_service_cache:check_status(OldClientName, OldClientAddr) of
        ?CONNECTED ->
            ok = chat_service_cache:drop_contact(OldClientName),
            ContactsList = [{<<"client_name">>, ClientName} ||
                ClientName <- chat_service_cache:get_connected_clients(), ClientName =/= OldClientName],
            ok = send_notifications(ContactsList,
                [{<<"type">>, <<"disconnected">>}, {<<"args">>, Args}]),
            <<"ok">>;
        Status ->
            Status
    end;
process_request(<<"get_contacts">> = ReqType, Args, Opts, ClientType) ->
    ThisClientName = proplists:get_value(<<"client_name">>, Args),
    ThisClientAddr = get_client_addr_from_req(ThisClientName, Opts, ClientType),
    case chat_service_cache:check_status(ThisClientName, ThisClientAddr) of
        ?CONNECTED ->
            ContactsList = [{<<"client_name">>, ClientName} ||
                ClientName <- chat_service_cache:get_connected_clients(), ClientName =/= ThisClientName],
            [{<<"type">>, ReqType}, {<<"args">>, ContactsList}];
        Status ->
            Status
    end;
process_request(<<"send_message">>, Args, Opts, ClientType) ->
    FromClientName = proplists:get_value(<<"from">>, Args),
    FromClientAddr = get_client_addr_from_req(FromClientName, Opts, ClientType),
    case chat_service_cache:check_status(FromClientName, FromClientAddr) of
        ?CONNECTED ->
            ToClientName = proplists:get_value(<<"to">>, Args),
            Msg = proplists:get_value(<<"msg">>, Args),
            ok = send_notifications([{<<"client_name">>, ToClientName}],
                [{<<"type">>, <<"new_message">>}, {<<"args">>, [{<<"from">>, FromClientName}, {<<"msg">>, Msg}]}]),
            <<"ok">>;
        Status ->
            Status
    end;
process_request(_, _, _, _) ->
    <<"ok">>.

%% INTERNAL

get_client_addr_from_req(ClientName, _Opts, erl) ->
    bin_to_atom(ClientName);
get_client_addr_from_req(_ClientName, Opts, tcp) ->
    proplists:get_value(<<"socket">>, Opts).

send_notifications([], _Msg) ->
    ok;
send_notifications([{_, ClientName}|Rest], Msg) ->
    case chat_service_cache:get_client_address(ClientName) of
        undefined ->
            ok;
        ClientAddr when is_atom(ClientAddr) ->
            ok = gen_server:cast({ClientAddr, ClientAddr}, encode(Msg));
        ClientAddr when is_port(ClientAddr) ->
            ranch_tcp:send(ClientAddr, encode(Msg))
    end,
    send_notifications(Rest, Msg).

bin_to_atom(Binary) ->
    try
        binary_to_existing_atom(Binary, latin1)
    catch
        _:_ ->
            binary_to_atom(Binary, latin1)
    end.
-module(chat_service_tcp_handler).
-behaviour(ranch_protocol).

-include("chat_service.hrl").

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    {OK, Closed, Error} = Transport:messages(),
    Transport:setopts(Socket, [{active, once}]),
    receive
        {OK, Socket, <<"\r\n">>} ->
            loop(Socket, Transport);
        {OK, Socket, Data} ->
            ?LOG_INFO("Socket:~p, Request via TCP: ~p~n", [Socket, Data]),
            PropList = chat_service_protocol:decode(Data),
            Type = proplists:get_value(<<"type">>, PropList, undefined),
            Args = proplists:get_value(<<"args">>, PropList, undefined),
            Response = chat_service_protocol:process_request(Type, Args, [{<<"socket">>, Socket}], tcp),
            RespBody = chat_service_protocol:encode(Response),
            ?LOG_INFO("Socket:~p, Response via TCP: ~p~n", [Socket, RespBody]),
            Transport:send(Socket, RespBody),
            loop(Socket, Transport);
        {Closed, Socket} ->
            ok;
        {Error, Socket, _} ->
            ok = Transport:close(Socket)
    end.

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
        {OK, Socket, Data} ->
            ?LOG_INFO("Socket:~p, Data via TCP: ~p~n", [Socket, Data]),
            Transport:send(Socket, Data),
            loop(Socket, Transport);
        {Closed, Socket} ->
            ok;
        {Error, Socket, _} ->
            ok = Transport:close(Socket)
    end.

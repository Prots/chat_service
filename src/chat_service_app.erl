-module(chat_service_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = chat_service_cache:init(),
    ok = set_cookie(),
    {ok, _} = start_acceptor_pool(),
    chat_service_sup:start_link().

stop(_State) ->
    ok.

%% Internal

set_cookie() ->
    {ok, Cookie} = application:get_env(chat_service, cookie),
    true = erlang:set_cookie(node(), Cookie),
    ok.

start_acceptor_pool() ->
    {ok, Port} = application:get_env(chat_service, port),
    {ok, PoolSize} = application:get_env(chat_service, tcp_pool_size),
    ranch:start_listener(my_pool, PoolSize,
        ranch_tcp, [{port, Port}],
        chat_service_tcp_handler, []).


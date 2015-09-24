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

handle_call(Request, _From, State) ->
    ?LOG_INFO("Call request: ~p~n", [Request]),
    Response = process_request(Request),
    ResponseBody = chat_service_protocol:encode(Response),
    ?LOG_INFO("Call response: ~p~n", [ResponseBody]),
    {reply, ResponseBody, State}.

handle_cast(Request, State) ->
    ?LOG_INFO("Cast request: ~p~n", [Request]),
    _ = process_request(Request),
    {noreply, State}.

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

process_request(Request) ->
    PropList = chat_service_protocol:decode(Request),
    Type = proplists:get_value(<<"type">>, PropList),
    Args = proplists:get_value(<<"args">>, PropList),
    chat_service_protocol:process_request(Type, Args, [], erl).

-module(chat_service_protocol).
-author("prots").

%% API
-export([decode/1]).
-export([encode/1]).

decode(Msg) when is_binary(Msg) ->
    process_request(bert:decode(Msg));
decode(Msg) ->
    Msg.

encode(Msg) ->
    bert:encode(Msg).


process_request({connect, _Socket, _Name}) ->
    ok;
process_request({disconnect, _Socket, _Name}) ->
    ok;
process_request({get_contacts, _Socket, _Name}) ->
    ok;
process_request({send_message, {_ToPid, _ToNode}, {_FromPid, _FromNode}, _Msg}) ->
    ok;
process_request(Msg) ->
    Msg.
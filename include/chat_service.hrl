-define(LOG_DEBUG(Format, Args),
    lager:debug(Format, Args)).
-define(LOG_INFO(Format, Args),
    lager:info(Format, Args)).
-define(LOG_WARNING(Format, Args),
    lager:warning(Format, Args)).
-define(LOG_ERROR(Format, Args),
    lager:error(Format, Args)).
-define(LOG_CRITICAL(Format, Args),
    lager:critical(Format, Args)).

-define(CONNECTIONS_TABLE, chat_service_connections).

-define(CONNECTED, <<"connected">>).
-define(DISCONNECTED, <<"disconnected">>).
-define(ADDR_IN_USE, <<"address in use">>).
-define(NAME_IN_USE, <<"name in use">>).



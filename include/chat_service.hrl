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
-define(MESSAGES_TABLE, chat_service_messages).

-define(CONNECTED, 1).
-define(DISCONNECTED, 0).



#!/bin/sh
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin \  -name chat_service@127.0.0.1 -config sys -s chat_service

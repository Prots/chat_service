#!/bin/sh
erl -pa $PWD/ebin/ $PWD/deps/*/ebin chat_client -name client3@127.0.0.1  -setcookie qwerty -s chat_client

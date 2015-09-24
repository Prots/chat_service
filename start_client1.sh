#!/bin/sh
erl -pa $PWD/ebin/ -pa $PWD/deps/*/ebin chat_client -name client1@127.0.0.1  -setcookie qwerty -s chat_client

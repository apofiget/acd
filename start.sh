#!/bin/sh

#export DISPLAY=localhost:10.0

export DISPLAY=:0.0

ulimit -n 4096

erl +pc unicode -pa apps/acd/ebin/ deps/jiffy/ebin/ deps/serial/ebin/  deps/srly/ebin/ deps/ibrowse/ebin/ deps/lager/ebin/ deps/yaws/ebin/ deps/goldrush/ebin/ -config app.config -sname acd -eval 'application:start(acd)'

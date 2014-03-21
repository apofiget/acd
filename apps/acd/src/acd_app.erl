-module(acd_app).

-behaviour(application).

-compile([{parse_transform, lager_transform}]).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lager:start(),
    gen_event:start_link({local,acd_evm}), 
    try [ acd_sup:start(A) || A <- [ar_com, fm, y_embed]] of
    	_-> {ok, self()}
    	catch _:X -> error_logger:error_msg("Error while start: ~p~n", [X])
    end. 

stop(_State) ->
    [exit(whereis(A) , normal) || A <- [ar_com_sup]].

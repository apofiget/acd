-module(ar_grbl_handler).

-behaviour(gen_event).

-compile([{parse_transform, lager_transform}]).

-export([code_change/3, handle_call/2, handle_event/2,
	 handle_info/2, init/1, terminate/2]).

init(_) -> lager:trace_console([{module, ?MODULE}], debug).

handle_event(Event, State) -> 
    lager:debug("~p~n", [Event]), 
{ok, State}.

handle_call(_Msg, State) -> {ok, [], State}.

handle_info(_Msg, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(Args, State) -> 
	lager:stop_trace(State),
	{stop, Args}.

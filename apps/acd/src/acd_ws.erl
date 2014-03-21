-module(acd_ws).

-include("include/include.hrl").

-include("../../../deps/yaws/include/yaws_api.hrl").

-export([init/1, handle_message/2, handle_open/2, handle_info/2, terminate/2]).

-record(state, {client,self,ws=""}).

%% @private
init([Arg,_]) -> 
{ClIP,Clp} = Arg#arg.client_ip_port,
Client = string:join([inet:ntoa(ClIP), integer_to_list(Clp) ], ":"),
{ok, #state{client=Client,self=self()}}.

%% @private
handle_open(WSState, State) -> 
    gen_event:add_sup_handler(acd_evm, {acd_ev_ws_handler, State#state.self}, State#state.self),
    error_logger:info_msg("Client connect: ~p~n", [State#state.client]), 
    {ok, State#state{ws=WSState}}.

%% @private
handle_message({text, <<"close">>}, State) ->
    gen_event:delete_handler(snms_ev, {snms_ev_ws_handler, State#state.self}, normal), 
    {close, normal};

handle_message({text, Message}, State) ->
    Cmd = decode_msg(Message),
    case Cmd of
        [] -> {reply, {text, list_to_binary(json2:encode(json2:obj_from_list([{status, "error"},{"error", "Unknow command"}])))},State};
        {command, ls} ->  
                {reply, {text, fm:ls()}, State};
        [{command, ls},{mask, Mask}] ->  
                {reply, {text, fm:ls(Mask)}, State};
        _ -> {noreply, State}
    end;

handle_message({close, Status, _Reason}, State) ->
    gen_event:delete_handler(snms_ev, {snms_ev_ws_handler, State#state.self}, normal), 
    {close, Status}.

%% @private
handle_info({event, {_EvCode, EvType, Ev}},State) ->
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    error_logger:info_msg("Client disconnect: ~p~n", [State#state.client]), 
    gen_event:delete_handler(snms_ev, {snms_ev_ws_handler, State#state.self}, normal),
    ok.


%% @private
decode_msg(M) ->
    try jiffy:decode(M)  of
        {[{<<"command">>,<<"ls">>}]} -> {command, ls};
        {[{<<"command">>,<<"ls">>},{<<"mask", Mask>>}]} -> [{command, ls},{mask, binary_to_list(Mask)}];
        E -> error_logger:error_msg("Msg: ~p~n", [E]), []
    catch _:E -> 
        error_logger:error_msg("Error ~p~n", [E]),
        []
    end. 

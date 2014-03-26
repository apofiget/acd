%% @author Andrey Andruschenko <apofiget@gmail.com>
%% @version 1.0
%% @doc Grbl API
%% Module works/tested with Arduino based on Grbl 0.8c firwmware.
%% @reference <a href="https://github.com/daapp/web-machining/blob/master/NOTES.org">Project refrence on Github.</a>
%% @end

-module(ar_grbl).

-compile([{parse_transform, lager_transform}]).

-include("include/include.hrl").

-behaviour(gen_server).

-export([start/0]).

-export([send/1,stop/0, firmware_version/0, mode/1, 
		 mode/0, current_status/0, reset_grbl/0, hold/0, cont/0, reply/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{
			  id :: integer()
			  ,command = "" :: string()
			  ,reply = "" :: string()
			  ,to="" :: {pid(), reference()} | []
			  ,mon_ref="" :: reference() | []
			  ,fin_state=idle :: idle | wait | hold
			  ,ver="" :: string() | []
			  ,mode=command :: command | file 
			  }).

%% @type arduino_reply() = Tuples
%%       Tuples = [Tuple]
%%       Tuple = {id,integer()} | {command, string()} | {reply,Reply}
%%       Reply = {status,Status} | {version, string()} | {R, I}
%%       R = ok | error | button | sensor | status | unknown_event
%%       I = string()
%%       Status = {{state,ArduinoStatus}, {position,ToolPosition}}
%%       ArduinoStatus = string()
%%       ToolPosition = string()

%% @spec start() -> {ok, pid()} | {error, Reason}
%% @doc Start gen_server
%% @end

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec stop() -> ok
%% @doc Stop gen_server
%% @end

stop() -> gen_server:call(?MODULE, {stop}).

%% @spec reply(S :: string) -> ok
%% @doc Get reply from serial port.
%% Function used as callback function for ar_serial, when data received from port.
%% Function should't be used manually!
%% @end

reply(S) -> gen_server:cast(?MODULE, {reply, S}).

%% @spec mode(Mode) -> ok | {ok, Id} | {error, daemon_locked}
%%			Id = integer()
%%			Mode = file | command
%% @doc Set gen_server internal mode.
%% In "file" mode, "send" function calls allowed only from process that set's "file" mode
%% In "command" mode, "send" function calls allowed from any process
%% @end

mode(M) -> gen_server:call(?MODULE, {mode, M}).

%% @spec mode() -> {ok, {command, {pid(), reference()}}} | {ok, {file,{pid(),reference()}}}
%% @doc Get current work mode
%% @end

mode() -> gen_server:call(?MODULE, {mode}).

%% @spec current_status() -> arduino_reply() | {error, daemon_locked} | {error, not_ready}
%% @doc Get current Arduino status and tool position
%% @end

current_status() -> gen_server:call(?MODULE, {send, "?"}, commons:get_opt(tty_timeout)).

%% @spec reset_grbl() -> arduino_reply() | {error, daemon_locked} | {error, not_ready}
%% @doc Make Arduino reset - send Ctrl+X to controller
%% @end

reset_grbl() -> gen_server:call(?MODULE, {send, "\^x"}, commons:get_opt(tty_timeout)).

%% @spec send(Cmd :: string()) -> arduino_reply() | {error, daemon_locked} | {error, not_ready}
%% @doc Send command to Arduino
%% @end

send(Cmd) -> gen_server:call(?MODULE, {send, Cmd}, commons:get_opt(tty_timeout)).

%% @spec hold() -> arduino_reply() | {error, daemon_locked} | {error, port_hold} | {error, not_ready}
%% @doc Arduino feed hold.
%% @end

hold() -> gen_server:call(?MODULE, {hold}).

%% @spec cont() -> arduino_reply() | {error, daemon_locked} | {error, not_ready}
%% @doc Arduino cyrcle start
%% @end


cont() -> gen_server:call(?MODULE, {cont}).

%% @spec firmware_version() -> {ok, string()} | {error, not_ready}
%% @doc Get Grbl version
%% @end

firmware_version() -> gen_server:call(?MODULE, {ver}). 

%% @hidden
init([]) -> {ok, #state{id=commons:id(4)}}.

%% @hidden
handle_call({mode}, From, #state{to = {P,_}} = State) when State#state.mode =:= file -> {reply, {ok, {file, P}}, State};
handle_call({mode}, From, State) -> {reply, {ok, {State#state.mode, State#state.to}}, State};

handle_call({mode, file}, {P,T} = From, State) when State#state.mode =:= command -> 
	Ref = monitor(process, P),
	{reply, {ok,State#state.id}, State#state{mode=file, to=From, mon_ref=Ref}};

handle_call({mode, file}, {P,_} = From, #state{to={P,_}} = State) when State#state.mode =:= file ->  {reply, {ok,State#state.id}, State};

handle_call({mode, command}, {P,_} = From, #state{to={F,_}} = State) when F =:= P, State#state.mode =:= file -> 
	demonitor(State#state.mon_ref),
	{reply, ok, State#state{mode=command, to="",mon_ref="",id = State#state.id + 1}};

handle_call({mode, command}, From, State) when State#state.mode =:= command -> {reply, ok, State};

handle_call({mode, M}, From, State) -> {reply, {error, daemon_locked}, State};

handle_call({ver}, _From, State) when State#state.ver =:= "" -> {reply, {error, not_ready}, State};
handle_call({ver}, _From, State) -> {reply, {ok, State#state.ver}, State};

handle_call({send, _Cmd}, _From, State) when State#state.fin_state =:= hold -> {reply, {error, not_ready}, State};

%handle_call({send, _Cmd}, _From, State) when State#state.ver =:= "" -> {reply, {error, not_ready}, State};

handle_call({send, Cmd}, {P,_} = From, #state{to={F,_}} = State) when F =/= P andalso State#state.mode =:= file -> 
	{reply, {error, daemon_locked}, State};

handle_call({send, _Cmd}, _From, State) when State#state.mode =:= command andalso State#state.fin_state =:= wait -> 
	{reply, {error, not_ready}, State};

handle_call({send, Cmd}, From, State) when State#state.fin_state =:= idle ->
		ar_serial:send(Cmd), 
		{noreply, State#state{to=From,fin_state=wait,command=Cmd}};

handle_call({hold}, From, State) when State#state.mode =:= file, State#state.to =/= From -> 
	{reply, {error, daemon_locked}, State};

handle_call({hold}, From, State) when State#state.fin_state =:= hold -> 
	{reply, {error, port_hold}, State};

handle_call({hold}, From, State) ->
	ar_serial:send("!\n"), 
	{noreply, State#state{to=From,fin_state=hold,command="!"}};

handle_call({cont}, From, State) when State#state.fin_state =:= file, State#state.to =/= From -> 
	{reply, {error, daemon_locked}, State};

handle_call({cont}, From, State) ->
	ar_serial:send("~\n"), 
	{noreply, State#state{to=From,fin_state=wait,command="~" }}; %% ~

handle_call({stop}, _From, State) ->
    {stop, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @hidden
handle_cast({reply, Data}, State) ->
	Str = State#state.reply ++ Data,
	case what_happens(Str) of
		{data, _D} -> 
				NewState = State#state{reply = Str};
		Any -> 
				io:format("Got reply: ~p~n", [Any]), 
				NewState = State#state{reply = ""},
				self() ! {data, Any}
	end, 
	{noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info({data, {banner, V}}, #state{to = {P,T}} = State) when State#state.fin_state =:= wait ->
		gen_server:reply(State#state.to, [{id, State#state.id},
											{command, State#state.command},
											{reply, {version , V}}]),
    	{noreply, State#state{ver=V}};

handle_info({data, {banner, V}}, State) ->
    	{noreply, State#state{ver=V}};

handle_info({data, {A, _V} = Event}, #state{to = {P,T}} = State) when State#state.fin_state =:= wait, A =:= button; A =:= sensor ->
		gen_server:reply(State#state.to, [{id, 0},
											{command, State#state.command},
											{reply, Event}]),
    	{noreply, State};

handle_info({data, {A, _V} = Event}, State) when  A =:= button; A =:= sensor ->
		gen_event:notify(acd_evm, Event), 
    	{noreply, State};


handle_info({data, {A, _V} = Event}, State) when  State#state.fin_state =:= idle ->
		gen_event:notify(acd_evm, Event), 
    	{noreply, State};

handle_info({data, Rsp}, State) when State#state.fin_state =:= hold ->
		gen_server:reply(State#state.to, [{id, State#state.id},
												{command, State#state.command},
												{reply, Rsp}]),
    	{noreply, State};

handle_info({data, Rsp}, State) ->
		gen_server:reply(State#state.to, [{id, State#state.id},
											{command, State#state.command},
											{reply, Rsp}]),
		Id = if State#state.mode =:= file -> State#state.id;
				true -> State#state.id + 1 end,
    	{noreply, State#state{fin_state=idle,id=Id}};

handle_info({'DOWN', Ref, process, _From, Info}, #state{mon_ref=Ref} = State) ->
	%%% wait for '<Idle' from Arduino, then send Ctrl+X
	{noreply, State#state{to="",mode=command,mon_ref="", fin_state=idle, id = State#state.id + 1}};

handle_info(_Msg, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @hidden
is_ok(E) ->
	case commons:re_match("(ok\r\n)$", E) of
        {match, [_,V]} ->
            {ok,E};
        nomatch ->
            false
    end.

%% @hidden
is_banner(E) ->
    case commons:re_match("Grbl\s+([^\s]+)\s+\\['\\$' for help\\]$", E) of
        {match, [_,V]} ->
            {banner,binary_to_list(V)};
        nomatch ->
            false
    end.

%% @hidden
is_error(E) ->
 	case string:tokens(E, ":") of
		["error", Error] -> {error, Error};
		_ -> false
	end.

%% @hidden
is_button(E) ->
 	case string:tokens(E, ":") of
		["button", Info] -> {button, Info};
		_ -> false
	end.

%% @hidden
is_sensor(E) ->
 	case string:tokens(E, ":") of
		["sensor", Info] -> {sensor, Info};
		_ -> false
	end.

%% @hidden
is_status(E) ->
 	case commons:re_match("^\<", E) of
		{match,_} -> 
			[H|T] = string:tokens(E, ","),
			Tl = string:join(T, ","), 
			Sp = string:str(Tl, ">") - 1,
			Pos = string:substr(Tl, 1, Sp),  
			{status, {{state,hd(string:tokens(H, "<"))},{position,Pos}}};
		_ -> false
	end.

%% @hidden
clr_1310([13,10|T]) -> T;
clr_1310(L) -> L.

%% @hidden
what_happens("ok") -> {ok,"ok"};
what_happens("ok\r\n") -> {ok,"ok"};
what_happens(Str) ->
	[H|T] = [
					fun(El) -> is_ok(El) end,
					fun(El) -> is_error(El) end,
					fun(El) -> is_status(El) end,
					fun(El) -> is_banner(El) end,
					fun(El) -> is_button(El) end,
					fun(El) -> is_sensor(El) end
			],
		what_happens(Str, H(Str), T).

%% @hidden
what_happens(Str,false,[H|T]) -> what_happens(Str,H(Str),T);
what_happens(Str,false,[]) -> {data, Str};
what_happens(Str,R,_L) -> R.
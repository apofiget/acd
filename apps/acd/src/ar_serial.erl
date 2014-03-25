%% @author Andrey Andruschenko <apofiget@gmail.com>
%% @version 1.1
%% @doc Arduino serial port low level communication.<br/>
%% Process communicate with Arduino via serial port that name placed in app.config as {tty, "/path/to/dev/file"}.<br/>
%% Module write data to serial port and read data from port per line.<br/>
%% @reference <a href="https://github.com/daapp/web-machining/blob/master/NOTES.org">Project refrence on Github.</a>
%% @end

-module(ar_serial).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

-export([read/3, send/1]).

-export([start/0, start/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state,
        {fd :: pid()
        %% low-level serial communication process PID
        ,reader :: pid()
        %% read from serial port process PID
        ,callback :: {atom(), atom()}
        %% callback {module, function}
        ,trace_file = "" :: string()}).     
        %% trace file name

%% @spec send(Cmd :: string()) -> ok
%% @doc Send command to serial port
%% @end
send(Cmd) -> gen_server:cast(?MODULE, {send, Cmd}).

%% @spec start(Params) -> {ok, Pid :: pid()} | {stop, Error :: any()}
%%      Params = {Module, Function, LineEndCharacters} | {Fun, LineEndCharacters}
%%      Module = atom()
%%      Function = atom()
%%      Fun = fun()
%%      LineEndCharacters = string()
%% @doc Start communication server, with given callback specified as {Module, Function, LineEndCharacters} <br/>
%% or {Fun, LineEndCharacters}.
%% @end

start({F,LineEnd}) when is_function(F) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{"", F, LineEnd}], []);

start({M,F,LineEnd}) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{M, F, LineEnd}], []).

%% @spec start() -> {ok, Pid :: pid()} | {stop, Error :: any()}
%% @doc Start communication server, with default callback specification - module ar_grbl, function reply and LineEnd - "\r\n".
%% @end

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{ar_grbl, reply, "\r\n"}], []).

%% @hidden
init([{M,F,LineEnd}]) ->
    process_flag(trap_exit, true),
    case serctl:open(commons:get_opt(tty)) of
        {ok, LowLevelPid} -> 
        Termios = lists:foldl(
            fun(Fun, Acc) -> Fun(Acc) end,
                serctl:mode(raw),
                [
                    fun(N) -> serctl:flow(N, false) end,
                    fun(N) -> serctl:ispeed(N, b9600) end,
                    fun(N) -> serctl:ospeed(N, b9600) end
                ]
            ),
            ReadProcess = spawn_link(ar_serial, read, [LowLevelPid,self(),LineEnd]),
            ok = serctl:tcsetattr(LowLevelPid, tcsanow, Termios),
            {ok, #state{fd = LowLevelPid, reader = ReadProcess, callback = {M, F}}};
        {error, Reason} -> {stop, Reason}
     end.

%% @hidden
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @hidden
handle_cast({send, Cmd}, #state{fd = LowLevelPid} = State) ->
    case serctl:write(LowLevelPid, list_to_binary(Cmd++["\n"])) of
        {error, Reason} ->  {stop, Reason, State};
        _ -> {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info({data, Str}, #state{callback = {_M,F}} = State) when is_function(F) ->
    F(Str),
    {noreply, State}; 

handle_info({data, Str}, #state{callback = {M,F}} = State) when is_atom(M), is_atom(F)  ->
    erlang:apply(M, F, [Str]),
    {noreply, State}; 

handle_info({'EXIT',Pid,Reason}, #state{reader = Pid, callback = {M,F}} = State) ->
    erlang:apply(M, F, [{error,Reason}]), 
    {stop, {port_close, Reason}, State};

handle_info(_Msg, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, State) ->
    serctl:close(State#state.fd), 
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @spec read(FD :: pid(), Parent :: pid(), LineEnd :: string()) -> ok
%% @doc Start serial port reader. Read data from serial port, that identified as low-level
%% process pid - FD, and send data when line ends to parent process.
%% Function should't be used manualy!
%% @end

read(FD, Parent, LineEnd) ->
    process_flag(trap_exit, true),
    read(FD, 0, [], Parent, LineEnd).

%% @hidden
read(FD, T, Acc, Parent, LineEnd) when T < 5000 ->
    case serctl:read(FD, 1) of
        {ok, Data} -> 
            Nacc = clear_1310(Acc ++ binary_to_list(Data)),
                case is_eol(Nacc, LineEnd) of 
                    false -> 
                        read(FD,0,Nacc,Parent, LineEnd);
                    true ->
                        Parent ! {data, Nacc},
                        read(FD, 0,[], Parent, LineEnd)
                end;
        {error, eagain} ->
            timer:sleep(10),
            read(FD,T+10, Acc, Parent, LineEnd);
        Error -> 
            if length(Acc) > 0 -> Parent ! {data, Acc};
                true -> {error, Error}
            end 
    end;

read(FD, _T, [], Parent, LineEnd) -> 
    read(FD,0,[],Parent, LineEnd);

read(FD, _T, Acc, Parent, LineEnd) -> 
    Parent ! {data, {unknown_event, Acc}},
    read(FD,0,[],Parent, LineEnd).

%% @hidden
is_eol(Str, LineEnd) ->
 case string:str(Str, LineEnd) - 1 of
    N when N > 0 -> true;
    _ -> false
    end.

%% @hidden
clear_1310([13,10|T]) -> T;
clear_1310(L) -> L.

%% @author Andrey Andruschenko <apofiget@gmail.com>
%% @version 1.0
%% @doc File manipulation API
%% @reference <a href="https://github.com/daapp/web-machining/blob/master/NOTES.org">Project refrence on Github.</a>
%% @end

-module(fm).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

-include("include/include.hrl").

-export([start/0, stop/0, ls/0, ls/1, del/1, rename/2, copy/2, run/1, 
		 runfile/2, lock/1, unlock/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(FS_BASE, commons:base_dir(acd) ++ "/priv/gcode/").

%% @spec ls() -> binary()
%% @doc Get directory listing where base directory is application install directory + "/priv/gcode"
%% @end

ls() -> gen_server:call(?MODULE, {ls, ?FS_BASE, "*"}). 

%% @spec ls(M :: string()) -> binary()
%% @doc Get directory listing with mask M , where base directory is application install directory + "/priv/gcode"
%% @end

ls(M) -> gen_server:call(?MODULE, {ls, ?FS_BASE, M}). 


%% @spec del(F :: string()) -> binary()
%% @doc Delete file or directory F , where base directory is application install directory + "/priv/gcode",
%% F may be file name, directory name or wildcard.
%% @end

del(F) -> gen_server:call(?MODULE, {delete, ?FS_BASE, F}).

%% @spec rename(From :: string(), To :: string()) -> binary()
%% @doc Rename source file From to destination file To , where base directory is application install directory + "/priv/gcode"
%% @end

rename(F,T) -> gen_server:call(?MODULE, {rename, F, T, ?FS_BASE}).

%% @spec copy(From :: string(), To :: string()) -> binary()
%% @doc Copy source file From to destination file To , where base directory is application install directory + "/priv/gcode"
%% @end

copy(F,T) -> gen_server:call(?MODULE, {copy, F, T, ?FS_BASE}).

%% @spec run(F :: string()) -> {ok, Id, binary()} | binary()
%% @doc Run given file on Arduino, return generated runid or some error.
%% @end

run(F) -> 
    case in_chroot(F) of
		false -> format_error(invalid_filename);
		true ->
			case proc_lib:start_link(?MODULE, runfile, [self(),F]) of
				{ok, Id} -> {ok, Id, list_to_binary(json2:encode(json2:obj_from_list([{status, "ok"},{runid, Id}])))};
				{error, E} -> format_error(E)
			end
	end.

%% @spec lock(F :: string()) -> ok | {error, Error}
%%		Error = posix() | already_locked | invalid_filename
%% @doc Lock file F for functions "rename" and "del", where base directory is application install directory + "/priv/gcode".
%% @end

lock(F) -> gen_server:call(?MODULE, {lock, F, ?FS_BASE}).

%% @spec unlock(F :: string()) -> ok | {error, Error}
%%		Error = posix() | invalid_filename
%% @doc Unlock file F , where base directory is application install directory + "/priv/gcode".
%% @end

unlock(F) -> gen_server:call(?MODULE, {unlock, F, ?FS_BASE}).

%% @hidden
stop() -> gen_server:call(?MODULE, {stop}).

%% @hidden
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @hidden
init([]) ->
    {ok, []}.

%% @hidden
handle_call({ls, D, M}, _From, State) ->
R = case in_chroot(M) of
	true -> file_info(filelib:wildcard(M, D));
	false -> format_error(invalid_whildcard)
end,
{reply, R, State};

handle_call({delete, D, F}, From, State) ->
R = case in_chroot(F) of
	true -> delete(filelib:wildcard(F, D));
	false -> format_error(invalid_whildcard)
end,
{reply, R, State};

handle_call({rename, F, T, D}, _From, State) ->
R = case {in_chroot(F), in_chroot(T)} of
		{true, true} -> make_if_safe(D, F, fun() -> 
												case file:rename(D ++ F, D ++ T) of
													ok -> list_to_binary(json2:encode(json2:obj_from_list([{status, "ok"}])));
													{errror, R} -> format_error(R)
												end
												end);
		_ -> format_error(invalid_filename)
	end,
{reply, R, State};

handle_call({copy, F, T, D}, _From, State) ->
R = case {in_chroot(F), in_chroot(T)} of
		{true, true} -> 
			case file:copy(D ++ F, D ++ T) of
				ok -> list_to_binary(json2:encode(json2:obj_from_list([{status, "ok"}])));
				{errror, E} -> format_error(E)
			end;
		_ -> format_error(invalid_filename)
	end,
{reply, R, State};

handle_call({lock, F, D}, _From, State) -> 
R = case in_chroot(F) of
		true -> 
			case file:read_file_info(D ++ filename:rootname(F) ++ ".lock")  of
					{ok, _I} -> {error, already_locked};
					{error, _E} -> file:write_file(D ++ filename:rootname(F) ++ ".lock", [<<>>])
			end;
		_ -> {error, invalid_filename}
	end,
{reply, R, State};

handle_call({unlock, F, D}, _From, State) -> 
R = case in_chroot(F) of
		true -> file:delete(D ++ filename:rootname(F) ++ ".lock");
		_ -> {error, invalid_filename}
	end,
{reply, R, State};

handle_call({stop}, _From, State) ->
    {stop, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% Internals

%% @hidden
file_info([]) -> list_to_binary(json2:encode(json2:obj_from_list([{"status","ok"},{"result",[]}])));
file_info(L) -> file_info(L,[]).
%% @hidden
file_info([],A) -> list_to_binary(json2:encode(json2:obj_from_list([{"status","ok"},{"result",[json2:obj_from_list(E) || E <- A]}])));
file_info([H|T],A) ->
	case file:read_file_info(?FS_BASE ++ H) of
		{ok, I} ->
			file_info(T,A ++ [[{type, atom_to_binary(I#file_info.type, utf8)},{name, H }, {size, I#file_info.size},{mtime, unicode:characters_to_binary(commons:date4js(I#file_info.mtime))}]]);
		{error,Reason} ->
			file_info(T,A ++ [[{type, "unknown"},{name, H}, {size, "unknown"},{mtime, "unknown"},{error,atom_to_binary(Reason, utf8)}]])
	end.

%% @hidden
in_chroot(Path) ->
case commons:re_match("(^\.\/)|(^\/)|(^\.\.\/)",Path) of
	nomatch -> true;
	{match, _} -> false
end.

%% @hidden
make_if_safe(D, N, F) ->
	L = D ++ filename:rootname(N) ++ ".lock",
	case filelib:is_regular(L) of
		true ->  format_error(file_locked);
	 	false -> F()
	 end. 
%% @hidden
delete([]) -> format_error(no_entry);
delete(L) -> delete(L, ?FS_BASE, ok).

%% @hidden
delete(_L, _D, {error, Reason}) -> format_error(Reason);
delete([], _D, R) -> list_to_binary(json2:encode(json2:obj_from_list([{status, "ok"}])));
delete([H|T], D, _R) ->
	case file:read_file_info(D ++ H)  of
		{ok, I} when I#file_info.type =:= regular -> delete(T,D, make_if_safe(D, H, fun() -> file:delete(D ++ H) end));
		{ok, I} when I#file_info.type =:= directory -> delete(T,D,file:del_dir(D ++ H));
		{error, Reason} -> {error, Reason};
		_-> delete(T,D,ok)
	end.

%% @hidden
runfile(P,F) ->
 LS = fm:lock(F),
 if LS =:= ok ->
	case file:open(?FS_BASE ++ F,[binary,read_ahead]) of
		{ok, Io} -> 
			case ar_grbl:mode(file) of
				{ok, Id} ->  
						proc_lib:init_ack(P, {ok, Id}),
						try line_by_line(Io, Id, F, 1) 
							catch _:E -> lager:error("Runtime error: ~p:~p - ~p~n", [?MODULE, ?LINE, E])
						end, 
						file:close(Io);
				R ->	
						file:close(Io),
						proc_lib:init_ack(P, R)
			end;
		{error, Reason} -> proc_lib:init_ack(P, {error, Reason})
	end,
	fm:unlock(F);
  true -> proc_lib:init_ack(P, LS)
end.

%% @hidden
line_by_line(Io, Id, F, L) -> 
	case file:read_line(Io) of
		eof ->
			gen_event:notify(acd_evm, {{runid, Id},
										{message,list_to_binary(json2:encode(json2:obj_from_list([{runid, Id},{file_line, F ++ ":" ++ integer_to_list(L)},{message, "eof"}])))}});
		{error, E} ->
			gen_event:notify(acd_evm, {{runid, Id},
										{message,list_to_binary(json2:encode(json2:obj_from_list([{runid, Id},{file_line, F ++ ":" ++ integer_to_list(L)},{message, atom_to_binary(E, utf8)}])))}});
		{ok, D} ->
			%io:format("Stream file ~p from ~p, Runid: ~p , Line: ~p/~p~n", [F, Io, Id, D, L]),
			case strip_line(D, fun(S) -> ar_grbl:send(commons:clear_lf(binary_to_list(S))) end ) of
				[] ->   gen_event:notify(acd_evm, {{runid, Id},
											{message,list_to_binary(json2:encode(json2:obj_from_list([{runid, Id},{file_line, F ++ ":" ++ integer_to_list(L)},{cmd, ""},{reply, ""}])))}}),
						line_by_line(Io, Id, F, L+1);
				[{id, Id},{command,Cmd}, {reply, {error, E}}] -> 
						gen_event:notify(acd_evm, {{runid, Id},
											{message,list_to_binary(json2:encode(json2:obj_from_list([{runid, Id},{file_line, F ++ ":" ++ integer_to_list(L)},{cmd, ""},{reply, E}])))}}),
						exit([{error, E},{file_line,{F,L}}]);
				[{id, 0},{command,Cmd}, {reply, Event}] ->
						gen_event:notify(acd_evm, {{runid, 0},{message,Event}}),
						exit([{event, Event},{file_line,{F,L}}]);
				[{id, Id},{command,Cmd}, {reply, {ok, "ok"}}] ->	
						gen_event:notify(acd_evm, {{runid, Id},
											{message,list_to_binary(json2:encode(json2:obj_from_list([{runid, Id},{file_line, F ++ ":" ++ integer_to_list(L)},{cmd, Cmd},{reply, "ok"}])))}}),
						line_by_line(Io, Id, F, L+1);
				{error, E} -> 
						gen_event:notify(acd_evm, {{runid, Id},
											{message,list_to_binary(json2:encode(json2:obj_from_list([{runid, Id},{file_line, F ++ ":" ++ integer_to_list(L)},{error, atom_to_binary(E, utf8) }])))}}),
						exit([{error, E},{file_line,{F,L}}])
			end
	end.

%% @hidden
strip_line(L, F) ->
	case re:replace(re:replace(L , "\\([^)]+\\)","",[global,{return,binary}]), "\s+","",[global,{return,binary}]) of
		<<"\n">> -> [];
		<<>> -> [];
		S -> F(S)
	end.

%% @hidden
format_error(E) -> list_to_binary(json2:encode(json2:obj_from_list([{status, "error"},{message, atom_to_binary(E, utf8)}]))).

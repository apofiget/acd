%% @author Andrey Andruschenko <apofiget@gmail.com>
%% @version 1.0
%% @doc Functions used in some part of application
%% @reference <a href="https://github.com/daapp/web-machining/blob/master/NOTES.org">Project refrence on Github.</a>
%% @end

-module(commons).

-include("include/include.hrl").

-compile(export_all).

-export([get_opt/1, ts/0, unix_ts/0, date4js/1, re_match/2, id/1, base_dir/1, clear_lf/1, clear_crlf/1]).

%% @spec get_opt(O :: atom()) -> any()
%% @doc
%% Return application configuration option value
%% @end

get_opt(Key) ->
    case application:get_env(Key) of
      undefined -> proplists:get_value(Key, ?DEFCONF);
      {ok, Val} -> Val
    end.

%% @spec ts() -> string()
%% @doc
%% Return current datetime as string in format "YYYYY-MM-DD HH:Mn:SS"
%% @end

ts() ->
    {{Y, M, D}, {H, Mm, S}} = calendar:local_time(),
    T = io_lib:format('~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b', [Y, M, D, H, Mm, S]),
    lists:flatten(T).

%% @spec unix_ts() -> integer()
%% @doc
%% Return current datetime as UnixTimestamp format
%% @end

unix_ts() ->
    {Mega, Seconds, _} = os:timestamp(),
    Mega * 1000000 + Seconds.

%% @spec date4js(Date :: {{Y :: integer(), M :: integer(), D :: integer()},{H :: integer(), Min :: integer(), Sec :: integer()}}) -> string()
%% @doc
%% Return given tuple datetime as formated string
%% @end

date4js({{Y, M, D}, {H, Min, S}}) ->
    lists:flatten(io_lib:format('~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b',
        [Y, M, D, H, Min, S])).

%% @spec re_match(R :: string(), S :: string()) -> nomatch | {match, list()}
%% @doc
%% Give regexp and string, apply, and return result
%% @end

re_match(Re,Subj) ->
  case re:compile(Re) of
    {ok, M} -> re:run(Subj, M, [{capture,all,binary},{newline, anycrlf}]);
    _ -> nomatch
  end.

%% @spec base_dir(A :: atom()) -> string()
%% @doc
%% Return base directory of given application or current directory
%% @end

base_dir(A) ->
    {ok, H} = file:get_cwd(),
    case code:lib_dir(A) of
        {error, _} -> H;
        D -> H ++ "/" ++ D
    end.

%% @spec id(I :: integer()) -> integer()
%% @doc
%% Return pseudo-random value with given length
%% @end

id(N) -> id(N,[]).

id(0,A) -> list_to_integer(string:join(A, ""));

id(N,A) -> id(N-1,A ++ [lists:nth(random:uniform(9),[integer_to_list(I) || I <- lists:seq(1,9)])]).

%% @spec clear_lf(S :: list()) -> list()
%% @doc
%% Clear LF character from given string
%% @end

clear_lf(L) -> clear_lf(L,[]).
clear_lf([10|T],A) -> clear_lf(T,A);
clear_lf([], A) -> A;
clear_lf([H|T], A) -> clear_lf(T,A ++ [H]).

%% @spec clear_crlf(S :: list()) -> list()
%% @doc
%% Clear LFCR  character sequence from given string
%% @end

clear_crlf(L) -> clear_crlf(L,[]).
clear_crlf([13,10|T],A) -> clear_crlf(T,A);
clear_crlf([], A) -> A;
clear_crlf([H|T], A) -> clear_crlf(T,A ++ [H]).

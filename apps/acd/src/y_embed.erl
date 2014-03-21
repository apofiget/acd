%% @author Andrey Andruschenko <apofiget@gmail.com>
%% @version 1.0
%% @doc Start Yaws in embed mode.
%% @reference <a href="https://github.com/daapp/web-machining/blob/master/NOTES.org">Project refrence on Github.</a>
%% @end

-module(y_embed).

-compile(export_all).

start() -> {ok, spawn(?MODULE, run, [])}.

run() ->
    Id = "yaws_emb",
    Dir = commons:base_dir(acd),
    GconfList = 
        [{id, Id},
         {server_signature, "Yaws"},
         {log_wrap_size, 1048576},
         {copy_error_log, false},
		 {logdir, Dir ++ "/priv/logs/www"},
		 {ebin_dir, [Dir ++ "/ebin"]}],
    Docroot = Dir ++ "/www",
    SconfList = 
        [{port, 8888}, 
         {access_log, true},
		 {dir_listings, false},
		 {servername, commons:get_opt(yaws_host)},
		 {listen, {0, 0, 0, 0}}, 
         {docroot, Docroot}],
    {ok, SCList, GC, ChildSpecs} =
	yaws_api:embedded_start_conf(Docroot, SconfList,
				     GconfList, Id),
    [supervisor:start_child(y_embed_sup, Ch)
     || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.

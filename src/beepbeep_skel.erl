%% This is a direct copy of mochiweb_skel I've only changed a few 
%% things for my purposes
%% @hidden

-module(beepbeep_skel).
-export([skelcopy/2]).
-include_lib("kernel/include/file.hrl").

%% External API

skelcopy(DestDir, InName) ->
    Name = string:to_lower(InName),
    ok = ensuredir(DestDir),
    LDst = case length(filename:dirname(DestDir)) of 
               1 -> %% handle case when dirname returns "/"
                   0;
               N ->
                   N + 1
           end,
    skelcopy(src(), DestDir, Name, LDst),
    %
    case os:type() of
        {win32,_} ->
            {ok, Cwd} = file:get_cwd(),
            mk_win_dir_syslink(Name, "beepbeep-src", DestDir, Cwd ++ "/../"),
            mk_win_dir_syslink(Name, "erlydtl-src", DestDir, Cwd ++ "/../deps/erlydtl"),
            mk_win_dir_syslink(Name, "mochiweb-src", DestDir,  Cwd ++ "/../deps/mochiweb"),
            mk_bat_file(Name, DestDir);
        {unix,_} ->
            ok = file:make_symlink(
               filename:join(filename:dirname(code:which(?MODULE)), ".."),
               filename:join([DestDir, Name, "deps", "beepbeep-src"])),
            ok = file:make_symlink(
               filename:join(filename:dirname(code:which(?MODULE)), "../deps/erlydtl"),
               filename:join([DestDir, Name, "deps", "erlydtl-src"])),
            ok = file:make_symlink(
               filename:join(filename:dirname(code:which(?MODULE)), "../deps/mochiweb"),
               filename:join([DestDir, Name, "deps", "mochiweb-src"]))
    end.

%% Internal API

%% @doc Make symbolik link in current directory on windows vista or highter
mk_win_dir_syslink(ProjectName, LinkName, DestDir, LinkTarget) ->
    S = (list_to_atom("cd "++ filename:join([DestDir, ProjectName, "deps"]) ++ "& mklink /D " ++ LinkName ++ " \"" ++ LinkTarget ++ "\"")),
    os:cmd(S),
    %io:format("~nname:~p~ntarget:~p~n~n", [LinkName, DestTarget]),
    %io:format("~n~p~n", [S]),
    ok.

%% @doc make .bat file to start dev server on windows
mk_bat_file(ProjectName, DestDir) ->
    Name = "start-server.bat",
    Content = "make \n"
"start werl -pa ebin deps/*/ebin -boot start_sasl -s " ++ ProjectName ++ " -s reloader",
    file:set_cwd(DestDir ++ "//" ++ ProjectName),
    {ok, IODevice} = file:open(Name, [write]), file:write(IODevice, Content), file:close(IODevice),
    ok.

src() ->
    Dir = filename:dirname(code:which(?MODULE)),
    filename:join(Dir, "../priv/skel").

skel() ->
    "skel".

skelcopy(Src, DestDir, Name, LDst) ->
    Dest = re:replace(filename:basename(Src),skel(), Name,  [global, {return, list}]),
    case file:read_file_info(Src) of
        {ok, #file_info{type=directory, mode=Mode}} ->
            Dir = DestDir ++ "/" ++ Dest,
            EDst = lists:nthtail(LDst, Dir),
            ok = ensuredir(Dir),
            ok = file:write_file_info(Dir, #file_info{mode=Mode}),
            {ok, Files} = file:list_dir(Src),
            io:format("~s/~n", [EDst]),
            lists:foreach(fun ("." ++ _) -> ok;
                              (F) ->
                                  skelcopy(filename:join(Src, F), 
                                           Dir,
                                           Name,
                                           LDst)
                          end,
                          Files),
            ok;
        {ok, #file_info{type=regular, mode=Mode}} ->
            OutFile = filename:join(DestDir, Dest),
            {ok, B} = file:read_file(Src),
            S = re:replace(binary_to_list(B), skel(), Name, [global, {return, list}] ),
            ok = file:write_file(OutFile, list_to_binary(S)),
            ok = file:write_file_info(OutFile, #file_info{mode=Mode}),
            io:format("    ~s~n", [filename:basename(Src)]),
            ok;
        {ok, _} ->
            io:format("ignored source file: ~p~n", [Src]),
            ok;
	 {error, _} ->
	    io:format("problem with ~p~n",[Src]),
	    ok
    end.

ensuredir(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        E ->
            E
    end.

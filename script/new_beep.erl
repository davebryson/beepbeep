#!/usr/bin/env escript

-export([main/1]).

%% Borrowed from the excellent MochiWeb

%% External API

main([Name]) ->
    main([Name, "."]);
main([Name, Dest]) ->
    ensure(),
    DestDir = filename:absname(Dest),
    ok = beepbeep_skel:skelcopy(DestDir, Name);
main(_) ->
    usage().

%% Internal API

ensure() ->
    code:add_patha(filename:join(filename:dirname(escript:script_name()),
                                 "../ebin")).

usage() ->
    io:format("usage: ~s name [destdir]~n",
              [filename:basename(escript:script_name())]),
    halt(1).



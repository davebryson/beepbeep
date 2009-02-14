%% @doc This code is adapted from the ewgi project:
%% http://code.google.com/p/ewgi/
%% In the future BeepBeep may use the ewgi interface
%% to support both mochiweb and yaws
%% @hidden
-module(mochiweb_env).

-export([setup_environment/1]).

-include("beepbeep.hrl").

setup_environment(Req) ->
    parse_request(Req).

nhdr(L) when is_atom(L) ->
    nhdr(atom_to_list(L));
nhdr(L) when is_binary(L) ->
    nhdr(binary_to_list(L));
nhdr(L) when is_list(L) ->
    underscoreize(L, []).

underscoreize([], S) ->
    lists:reverse(S);
underscoreize([$-|T], S) ->
    underscoreize(T, [$_|S]);
underscoreize([H|T], S) ->
    underscoreize(T, [H|S]).

normalize_header({K, V}) ->
    {string:to_upper(string:strip(nhdr(K))), string:strip(V)}.

parse_request(Req) ->
    Hdrs = parse_headers(Req),
    lists:foldl(fun({El, ElName}, PList) ->
                        V = parse_element(El, Req),
                        case V of
                            undefined -> PList;
                            V ->
                                NewEl = proplists:property({ElName, V}),
                                [NewEl|PList]
                        end
                end, Hdrs, ?BEEPBEEP_ENV_DATA).

parse_element(server_sw, _Req) ->
    "MochiWeb";
parse_element(server_name, Req) ->
    HostPort = Req:get_header_value(host),
    case HostPort of
        HostPort when is_list(HostPort) ->
            hd(string:tokens(HostPort, ":"));
        HostPort -> HostPort
    end;
parse_element(server_port, Req) ->
    HostPort = Req:get_header_value(host),
    case HostPort of
        HostPort when is_list(HostPort) ->
            case length(HostPort) of
                2 -> lists:nth(2, HostPort);
                _ -> undefined
            end;
        _ ->
            undefined
    end;
parse_element(server_protocol, Req) ->
    {Maj, Min} = Req:get(version),
    lists:flatten(io_lib:format("HTTP/~b.~b", [Maj, Min]));
parse_element(method, Req) ->
    Req:get(method);
parse_element(path_info,Req) ->
    Req:get(path);
parse_element(remote_addr, Req) ->
    Req:get(peer);
parse_element(beepbeep_params,Req) ->
    case Req:get(method) of
	Method when Method =:= 'GET'; Method =:= 'HEAD' ->
	    Req:parse_qs(); 
	_ ->
	    Req:parse_post()
    end;
parse_element(content_type, Req) ->
    Req:get_header_value("content-type");
parse_element(content_length, Req) ->
    case Req:get_header_value("content-length") of
        undefined -> undefined;
        Length when is_integer(Length) ->
            Length;
        Length when is_list(Length) ->
            list_to_integer(Length)
    end.

parse_headers(Req) ->
    Hdrs = Req:get(headers),
    lists:foldl(fun(Pair, Acc) ->
                        {K1, V1} = normalize_header(Pair),
                        %% Don't duplicate content-length and content-type
                        case K1 of
                            "CONTENT_LENGTH" ->
                                Acc;
                            "CONTENT_TYPE" ->
                                Acc;
                            K1 ->
                                [{lists:append(["HTTP_", K1]), V1}|Acc]
                        end
                end,
                [],
                mochiweb_headers:to_list(Hdrs)).
    
			       

-module(erlydtl_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).

-file("/usr/local/lib/erlang/lib/parsetools-1.4.3/include/yeccpre.hrl", 0).
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id $
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    yeccpars0(Tokens, false).

parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {F, A});
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{M, F}, A}).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser
% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function,{return_error,2}}).
-spec(return_error/2 :: (integer(), any()) -> no_return()).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.2").

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                {syntax_error, Token} ->
                    yeccerror(Token);
                {missing_in_goto_table=Tag, State} ->
                    Desc = {State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace);
                {missing_in_goto_table=Tag, Symbol, State} ->
                    Desc = {Symbol, State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
            Error % probably from return_error/2
    end.

yecc_error_type(function_clause, [{?MODULE,F,[_,_,_,_,Token,_,_]} | _]) ->
    "yeccpars2" ++ _ = atom_to_list(F),
    {syntax_error, Token};
yecc_error_type({case_clause,{State}}, [{?MODULE,yeccpars2,_}|_]) ->
    %% Inlined goto-function
    {missing_in_goto_table, State};
yecc_error_type(function_clause, [{?MODULE,F,[State]}|_]) ->
    "yeccgoto_" ++ SymbolL = atom_to_list(F),
    {ok,[{atom,_,Symbol}]} = erl_scan:string(SymbolL),
    {missing_in_goto_table, Symbol, State}.

yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, 
              Tokenizer);
yeccpars1([], {F, A}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(Tokens, {F, A}, State, States, Vstack);
        {eof, _Endline} ->
            yeccpars1([], false, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], false, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Stack1, [Token | Tokens], 
          Tokenizer) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Stack1 | Vstack], Token, Tokens, Tokenizer);
yeccpars1(State1, State, States, Vstack, Stack1, [], {F, A}) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(State1, State, States, Vstack, Stack1, Tokens, {F, A});
        {eof, _Endline} ->
            yeccpars1(State1, State, States, Vstack, Stack1, [], false);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1(State1, State, States, Vstack, Stack1, [], false) ->
    yeccpars2(State, '$end', [State1 | States], [Stack1 | Vstack],
              {'$end', 999999}, [], false).

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);
yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/erlydtl/erlydtl_parser.erl", 148).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.2",{missing_state_in_action_table, Other}}).

yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2(1, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_1(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_1(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr).

yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_3_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 yeccpars2(153, Cat, [6 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccpars2(144, Cat, [8 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccpars2(133, Cat, [10 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 yeccpars2(128, Cat, [12 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_13_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_14_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_15_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 yeccpars2(123, Cat, [17 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_18_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_20_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccpars2(118, Cat, [21 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_23_(Stack),
 yeccpars2(113, Cat, [23 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_24_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_25(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, cycle_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, now_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr).

yeccpars2_26(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(S, number_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Value\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(S, close_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Value\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 yeccpars2('yeccgoto_\'Variable\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ValueBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_35(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr).

yeccpars2_36(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_37_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Value\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_38(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccpars2('yeccgoto_\'Filter\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_39(S, number_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_40_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Filter\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Variable\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_42(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr).

yeccpars2_43(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr).

yeccpars2_44(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr).

yeccpars2_45(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr).

yeccpars2_46(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, number_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr).

yeccpars2_47(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr).

yeccpars2_48(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_49_(Stack),
 yeccpars2(77, Cat, [49 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_50(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, not_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, number_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_51: see yeccpars2_26

%% yeccpars2_52: see yeccpars2_26

yeccpars2_53(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr).

yeccpars2_54(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr).

yeccpars2_55(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr).

yeccpars2_56(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_57_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'NowTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_58(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_59_(Stack),
 yeccpars2('yeccgoto_\'LoadNames\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_60_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'LoadTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_61_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'LoadNames\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_62(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_63_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'IncludeTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_64(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'IfNotEqualExpression\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_65: see yeccpars2_26

yeccpars2_66(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr).

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_67_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2('yeccgoto_\'IfNotEqualBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_68(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'IfEqualExpression\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_69: see yeccpars2_26

yeccpars2_70(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_71_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2('yeccgoto_\'IfEqualBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_72(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'IfExpression\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_73(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_74: see yeccpars2_50

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_75_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'IfExpression\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_76_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'IfBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_77(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_78_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'CustomTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_79(S, equal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_80: see yeccpars2_26

yeccpars2_81(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_81_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'Args\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_82(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, in_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_83(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_84_(Stack),
 yeccpars2('yeccgoto_\'ForGroup\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_85_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'ForBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_86(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_87: see yeccpars2_26

yeccpars2_88(_S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_88_close_tag(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ForExpression\''(hd(Nss)), close_tag, Nss, NewStack, T, Ts, Tzr);
yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Value\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_89(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr).

yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_90_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ForGroup\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_91(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr).

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_92_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'ExtendsTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_93(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_(Stack),
 yeccpars2('yeccgoto_\'CycleNames\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_94(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr).

yeccpars2_95(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, number_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr).

yeccpars2_96(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 yeccpars2('yeccgoto_\'Variable\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_97_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'CycleNamesCompat\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_98(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'CycleNames\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'CycleTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_100_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'CycleTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_101(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'CycleNamesCompat\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'CycleNamesCompat\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'CommentBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_104(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, with_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr).

yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'CallTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_106: see yeccpars2_26

yeccpars2_107(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr).

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_108_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2('yeccgoto_\'CallWithTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_109(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr).

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_110_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'BlockBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_111(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr).

yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_112_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'AutoEscapeBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_113(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr).

yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'AutoEscapeBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_115(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, cycle_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, endautoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, now_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr).

yeccpars2_116(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr).

yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndAutoEscapeBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_118(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr).

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_119_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'BlockBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_120(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, cycle_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, endblock_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, now_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr).

yeccpars2_121(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr).

yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_122_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndBlockBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_123(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr).

yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_124_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'CommentBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_125(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, cycle_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, endcomment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, now_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr).

yeccpars2_126(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr).

yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_127_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndCommentBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_128(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr).

yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_129_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ForBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_130(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, cycle_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, endfor_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, now_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr).

yeccpars2_131(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr).

yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_132_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndForBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_133(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr).

yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_134_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'IfBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_(Stack),
 yeccpars2(141, Cat, [135 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_136(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, cycle_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, else_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, endif_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, now_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr).

yeccpars2_137(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr).

yeccpars2_138(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr).

yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_139_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndIfBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_140_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ElseBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_141(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr).

yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_142_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2('yeccgoto_\'IfBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_143(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, cycle_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, endif_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, now_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr).

yeccpars2_144(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr).

yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_145_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'IfEqualBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_146_(Stack),
 yeccpars2(150, Cat, [146 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_147(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, cycle_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, else_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, endifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, now_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr).

yeccpars2_148(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr).

yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_149_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndIfEqualBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_150(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr).

yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_151_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2('yeccgoto_\'IfEqualBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_152(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, cycle_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, endifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, now_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr).

yeccpars2_153(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr).

yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_154_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'IfNotEqualBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_155_(Stack),
 yeccpars2(159, Cat, [155 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_156(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, cycle_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, else_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, endifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, now_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr).

yeccpars2_157(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr).

yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_158_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndIfNotEqualBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_159(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr).

yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_160_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2('yeccgoto_\'IfNotEqualBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_161(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, cycle_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, endifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, now_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Args\''(49) -> 77.

'yeccgoto_\'AutoEscapeBlock\''(1) -> 24;
'yeccgoto_\'AutoEscapeBlock\''(113) -> 24;
'yeccgoto_\'AutoEscapeBlock\''(118) -> 24;
'yeccgoto_\'AutoEscapeBlock\''(123) -> 24;
'yeccgoto_\'AutoEscapeBlock\''(128) -> 24;
'yeccgoto_\'AutoEscapeBlock\''(133) -> 24;
'yeccgoto_\'AutoEscapeBlock\''(141) -> 24;
'yeccgoto_\'AutoEscapeBlock\''(144) -> 24;
'yeccgoto_\'AutoEscapeBlock\''(150) -> 24;
'yeccgoto_\'AutoEscapeBlock\''(153) -> 24;
'yeccgoto_\'AutoEscapeBlock\''(159) -> 24.

'yeccgoto_\'AutoEscapeBraced\''(1) -> 23;
'yeccgoto_\'AutoEscapeBraced\''(113) -> 23;
'yeccgoto_\'AutoEscapeBraced\''(118) -> 23;
'yeccgoto_\'AutoEscapeBraced\''(123) -> 23;
'yeccgoto_\'AutoEscapeBraced\''(128) -> 23;
'yeccgoto_\'AutoEscapeBraced\''(133) -> 23;
'yeccgoto_\'AutoEscapeBraced\''(141) -> 23;
'yeccgoto_\'AutoEscapeBraced\''(144) -> 23;
'yeccgoto_\'AutoEscapeBraced\''(150) -> 23;
'yeccgoto_\'AutoEscapeBraced\''(153) -> 23;
'yeccgoto_\'AutoEscapeBraced\''(159) -> 23.

'yeccgoto_\'BlockBlock\''(1) -> 22;
'yeccgoto_\'BlockBlock\''(113) -> 22;
'yeccgoto_\'BlockBlock\''(118) -> 22;
'yeccgoto_\'BlockBlock\''(123) -> 22;
'yeccgoto_\'BlockBlock\''(128) -> 22;
'yeccgoto_\'BlockBlock\''(133) -> 22;
'yeccgoto_\'BlockBlock\''(141) -> 22;
'yeccgoto_\'BlockBlock\''(144) -> 22;
'yeccgoto_\'BlockBlock\''(150) -> 22;
'yeccgoto_\'BlockBlock\''(153) -> 22;
'yeccgoto_\'BlockBlock\''(159) -> 22.

'yeccgoto_\'BlockBraced\''(1) -> 21;
'yeccgoto_\'BlockBraced\''(113) -> 21;
'yeccgoto_\'BlockBraced\''(118) -> 21;
'yeccgoto_\'BlockBraced\''(123) -> 21;
'yeccgoto_\'BlockBraced\''(128) -> 21;
'yeccgoto_\'BlockBraced\''(133) -> 21;
'yeccgoto_\'BlockBraced\''(141) -> 21;
'yeccgoto_\'BlockBraced\''(144) -> 21;
'yeccgoto_\'BlockBraced\''(150) -> 21;
'yeccgoto_\'BlockBraced\''(153) -> 21;
'yeccgoto_\'BlockBraced\''(159) -> 21.

'yeccgoto_\'CallTag\''(1) -> 20;
'yeccgoto_\'CallTag\''(113) -> 20;
'yeccgoto_\'CallTag\''(118) -> 20;
'yeccgoto_\'CallTag\''(123) -> 20;
'yeccgoto_\'CallTag\''(128) -> 20;
'yeccgoto_\'CallTag\''(133) -> 20;
'yeccgoto_\'CallTag\''(141) -> 20;
'yeccgoto_\'CallTag\''(144) -> 20;
'yeccgoto_\'CallTag\''(150) -> 20;
'yeccgoto_\'CallTag\''(153) -> 20;
'yeccgoto_\'CallTag\''(159) -> 20.

'yeccgoto_\'CallWithTag\''(1) -> 19;
'yeccgoto_\'CallWithTag\''(113) -> 19;
'yeccgoto_\'CallWithTag\''(118) -> 19;
'yeccgoto_\'CallWithTag\''(123) -> 19;
'yeccgoto_\'CallWithTag\''(128) -> 19;
'yeccgoto_\'CallWithTag\''(133) -> 19;
'yeccgoto_\'CallWithTag\''(141) -> 19;
'yeccgoto_\'CallWithTag\''(144) -> 19;
'yeccgoto_\'CallWithTag\''(150) -> 19;
'yeccgoto_\'CallWithTag\''(153) -> 19;
'yeccgoto_\'CallWithTag\''(159) -> 19.

'yeccgoto_\'CommentBlock\''(1) -> 18;
'yeccgoto_\'CommentBlock\''(113) -> 18;
'yeccgoto_\'CommentBlock\''(118) -> 18;
'yeccgoto_\'CommentBlock\''(123) -> 18;
'yeccgoto_\'CommentBlock\''(128) -> 18;
'yeccgoto_\'CommentBlock\''(133) -> 18;
'yeccgoto_\'CommentBlock\''(141) -> 18;
'yeccgoto_\'CommentBlock\''(144) -> 18;
'yeccgoto_\'CommentBlock\''(150) -> 18;
'yeccgoto_\'CommentBlock\''(153) -> 18;
'yeccgoto_\'CommentBlock\''(159) -> 18.

'yeccgoto_\'CommentBraced\''(1) -> 17;
'yeccgoto_\'CommentBraced\''(113) -> 17;
'yeccgoto_\'CommentBraced\''(118) -> 17;
'yeccgoto_\'CommentBraced\''(123) -> 17;
'yeccgoto_\'CommentBraced\''(128) -> 17;
'yeccgoto_\'CommentBraced\''(133) -> 17;
'yeccgoto_\'CommentBraced\''(141) -> 17;
'yeccgoto_\'CommentBraced\''(144) -> 17;
'yeccgoto_\'CommentBraced\''(150) -> 17;
'yeccgoto_\'CommentBraced\''(153) -> 17;
'yeccgoto_\'CommentBraced\''(159) -> 17.

'yeccgoto_\'CustomTag\''(1) -> 16;
'yeccgoto_\'CustomTag\''(113) -> 16;
'yeccgoto_\'CustomTag\''(118) -> 16;
'yeccgoto_\'CustomTag\''(123) -> 16;
'yeccgoto_\'CustomTag\''(128) -> 16;
'yeccgoto_\'CustomTag\''(133) -> 16;
'yeccgoto_\'CustomTag\''(141) -> 16;
'yeccgoto_\'CustomTag\''(144) -> 16;
'yeccgoto_\'CustomTag\''(150) -> 16;
'yeccgoto_\'CustomTag\''(153) -> 16;
'yeccgoto_\'CustomTag\''(159) -> 16.

'yeccgoto_\'CycleNames\''(46) -> 95.

'yeccgoto_\'CycleNamesCompat\''(46) -> 94.

'yeccgoto_\'CycleTag\''(1) -> 15;
'yeccgoto_\'CycleTag\''(113) -> 15;
'yeccgoto_\'CycleTag\''(118) -> 15;
'yeccgoto_\'CycleTag\''(123) -> 15;
'yeccgoto_\'CycleTag\''(128) -> 15;
'yeccgoto_\'CycleTag\''(133) -> 15;
'yeccgoto_\'CycleTag\''(141) -> 15;
'yeccgoto_\'CycleTag\''(144) -> 15;
'yeccgoto_\'CycleTag\''(150) -> 15;
'yeccgoto_\'CycleTag\''(153) -> 15;
'yeccgoto_\'CycleTag\''(159) -> 15.

'yeccgoto_\'Elements\''(0) -> 1;
'yeccgoto_\'Elements\''(6) -> 153;
'yeccgoto_\'Elements\''(8) -> 144;
'yeccgoto_\'Elements\''(10) -> 133;
'yeccgoto_\'Elements\''(12) -> 128;
'yeccgoto_\'Elements\''(17) -> 123;
'yeccgoto_\'Elements\''(21) -> 118;
'yeccgoto_\'Elements\''(23) -> 113;
'yeccgoto_\'Elements\''(135) -> 141;
'yeccgoto_\'Elements\''(146) -> 150;
'yeccgoto_\'Elements\''(155) -> 159.

'yeccgoto_\'ElseBraced\''(133) -> 135;
'yeccgoto_\'ElseBraced\''(144) -> 146;
'yeccgoto_\'ElseBraced\''(153) -> 155.

'yeccgoto_\'EndAutoEscapeBraced\''(113) -> 114.

'yeccgoto_\'EndBlockBraced\''(118) -> 119.

'yeccgoto_\'EndCommentBraced\''(123) -> 124.

'yeccgoto_\'EndForBraced\''(128) -> 129.

'yeccgoto_\'EndIfBraced\''(133) -> 134;
'yeccgoto_\'EndIfBraced\''(141) -> 142.

'yeccgoto_\'EndIfEqualBraced\''(144) -> 145;
'yeccgoto_\'EndIfEqualBraced\''(150) -> 151.

'yeccgoto_\'EndIfNotEqualBraced\''(153) -> 154;
'yeccgoto_\'EndIfNotEqualBraced\''(159) -> 160.

'yeccgoto_\'ExtendsTag\''(1) -> 14;
'yeccgoto_\'ExtendsTag\''(113) -> 14;
'yeccgoto_\'ExtendsTag\''(118) -> 14;
'yeccgoto_\'ExtendsTag\''(123) -> 14;
'yeccgoto_\'ExtendsTag\''(128) -> 14;
'yeccgoto_\'ExtendsTag\''(133) -> 14;
'yeccgoto_\'ExtendsTag\''(141) -> 14;
'yeccgoto_\'ExtendsTag\''(144) -> 14;
'yeccgoto_\'ExtendsTag\''(150) -> 14;
'yeccgoto_\'ExtendsTag\''(153) -> 14;
'yeccgoto_\'ExtendsTag\''(159) -> 14.

'yeccgoto_\'Filter\''(36) -> 37.

'yeccgoto_\'ForBlock\''(1) -> 13;
'yeccgoto_\'ForBlock\''(113) -> 13;
'yeccgoto_\'ForBlock\''(118) -> 13;
'yeccgoto_\'ForBlock\''(123) -> 13;
'yeccgoto_\'ForBlock\''(128) -> 13;
'yeccgoto_\'ForBlock\''(133) -> 13;
'yeccgoto_\'ForBlock\''(141) -> 13;
'yeccgoto_\'ForBlock\''(144) -> 13;
'yeccgoto_\'ForBlock\''(150) -> 13;
'yeccgoto_\'ForBlock\''(153) -> 13;
'yeccgoto_\'ForBlock\''(159) -> 13.

'yeccgoto_\'ForBraced\''(1) -> 12;
'yeccgoto_\'ForBraced\''(113) -> 12;
'yeccgoto_\'ForBraced\''(118) -> 12;
'yeccgoto_\'ForBraced\''(123) -> 12;
'yeccgoto_\'ForBraced\''(128) -> 12;
'yeccgoto_\'ForBraced\''(133) -> 12;
'yeccgoto_\'ForBraced\''(141) -> 12;
'yeccgoto_\'ForBraced\''(144) -> 12;
'yeccgoto_\'ForBraced\''(150) -> 12;
'yeccgoto_\'ForBraced\''(153) -> 12;
'yeccgoto_\'ForBraced\''(159) -> 12.

'yeccgoto_\'ForExpression\''(48) -> 83.

'yeccgoto_\'ForGroup\''(48) -> 82.

'yeccgoto_\'IfBlock\''(1) -> 11;
'yeccgoto_\'IfBlock\''(113) -> 11;
'yeccgoto_\'IfBlock\''(118) -> 11;
'yeccgoto_\'IfBlock\''(123) -> 11;
'yeccgoto_\'IfBlock\''(128) -> 11;
'yeccgoto_\'IfBlock\''(133) -> 11;
'yeccgoto_\'IfBlock\''(141) -> 11;
'yeccgoto_\'IfBlock\''(144) -> 11;
'yeccgoto_\'IfBlock\''(150) -> 11;
'yeccgoto_\'IfBlock\''(153) -> 11;
'yeccgoto_\'IfBlock\''(159) -> 11.

'yeccgoto_\'IfBraced\''(1) -> 10;
'yeccgoto_\'IfBraced\''(113) -> 10;
'yeccgoto_\'IfBraced\''(118) -> 10;
'yeccgoto_\'IfBraced\''(123) -> 10;
'yeccgoto_\'IfBraced\''(128) -> 10;
'yeccgoto_\'IfBraced\''(133) -> 10;
'yeccgoto_\'IfBraced\''(141) -> 10;
'yeccgoto_\'IfBraced\''(144) -> 10;
'yeccgoto_\'IfBraced\''(150) -> 10;
'yeccgoto_\'IfBraced\''(153) -> 10;
'yeccgoto_\'IfBraced\''(159) -> 10.

'yeccgoto_\'IfEqualBlock\''(1) -> 9;
'yeccgoto_\'IfEqualBlock\''(113) -> 9;
'yeccgoto_\'IfEqualBlock\''(118) -> 9;
'yeccgoto_\'IfEqualBlock\''(123) -> 9;
'yeccgoto_\'IfEqualBlock\''(128) -> 9;
'yeccgoto_\'IfEqualBlock\''(133) -> 9;
'yeccgoto_\'IfEqualBlock\''(141) -> 9;
'yeccgoto_\'IfEqualBlock\''(144) -> 9;
'yeccgoto_\'IfEqualBlock\''(150) -> 9;
'yeccgoto_\'IfEqualBlock\''(153) -> 9;
'yeccgoto_\'IfEqualBlock\''(159) -> 9.

'yeccgoto_\'IfEqualBraced\''(1) -> 8;
'yeccgoto_\'IfEqualBraced\''(113) -> 8;
'yeccgoto_\'IfEqualBraced\''(118) -> 8;
'yeccgoto_\'IfEqualBraced\''(123) -> 8;
'yeccgoto_\'IfEqualBraced\''(128) -> 8;
'yeccgoto_\'IfEqualBraced\''(133) -> 8;
'yeccgoto_\'IfEqualBraced\''(141) -> 8;
'yeccgoto_\'IfEqualBraced\''(144) -> 8;
'yeccgoto_\'IfEqualBraced\''(150) -> 8;
'yeccgoto_\'IfEqualBraced\''(153) -> 8;
'yeccgoto_\'IfEqualBraced\''(159) -> 8.

'yeccgoto_\'IfEqualExpression\''(51) -> 69.

'yeccgoto_\'IfExpression\''(50) -> 73;
'yeccgoto_\'IfExpression\''(74) -> 75.

'yeccgoto_\'IfNotEqualBlock\''(1) -> 7;
'yeccgoto_\'IfNotEqualBlock\''(113) -> 7;
'yeccgoto_\'IfNotEqualBlock\''(118) -> 7;
'yeccgoto_\'IfNotEqualBlock\''(123) -> 7;
'yeccgoto_\'IfNotEqualBlock\''(128) -> 7;
'yeccgoto_\'IfNotEqualBlock\''(133) -> 7;
'yeccgoto_\'IfNotEqualBlock\''(141) -> 7;
'yeccgoto_\'IfNotEqualBlock\''(144) -> 7;
'yeccgoto_\'IfNotEqualBlock\''(150) -> 7;
'yeccgoto_\'IfNotEqualBlock\''(153) -> 7;
'yeccgoto_\'IfNotEqualBlock\''(159) -> 7.

'yeccgoto_\'IfNotEqualBraced\''(1) -> 6;
'yeccgoto_\'IfNotEqualBraced\''(113) -> 6;
'yeccgoto_\'IfNotEqualBraced\''(118) -> 6;
'yeccgoto_\'IfNotEqualBraced\''(123) -> 6;
'yeccgoto_\'IfNotEqualBraced\''(128) -> 6;
'yeccgoto_\'IfNotEqualBraced\''(133) -> 6;
'yeccgoto_\'IfNotEqualBraced\''(141) -> 6;
'yeccgoto_\'IfNotEqualBraced\''(144) -> 6;
'yeccgoto_\'IfNotEqualBraced\''(150) -> 6;
'yeccgoto_\'IfNotEqualBraced\''(153) -> 6;
'yeccgoto_\'IfNotEqualBraced\''(159) -> 6.

'yeccgoto_\'IfNotEqualExpression\''(52) -> 65.

'yeccgoto_\'IncludeTag\''(1) -> 5;
'yeccgoto_\'IncludeTag\''(113) -> 5;
'yeccgoto_\'IncludeTag\''(118) -> 5;
'yeccgoto_\'IncludeTag\''(123) -> 5;
'yeccgoto_\'IncludeTag\''(128) -> 5;
'yeccgoto_\'IncludeTag\''(133) -> 5;
'yeccgoto_\'IncludeTag\''(141) -> 5;
'yeccgoto_\'IncludeTag\''(144) -> 5;
'yeccgoto_\'IncludeTag\''(150) -> 5;
'yeccgoto_\'IncludeTag\''(153) -> 5;
'yeccgoto_\'IncludeTag\''(159) -> 5.

'yeccgoto_\'Literal\''(26) -> 30;
'yeccgoto_\'Literal\''(39) -> 40;
'yeccgoto_\'Literal\''(46) -> 30;
'yeccgoto_\'Literal\''(50) -> 30;
'yeccgoto_\'Literal\''(51) -> 30;
'yeccgoto_\'Literal\''(52) -> 30;
'yeccgoto_\'Literal\''(65) -> 30;
'yeccgoto_\'Literal\''(69) -> 30;
'yeccgoto_\'Literal\''(74) -> 30;
'yeccgoto_\'Literal\''(80) -> 30;
'yeccgoto_\'Literal\''(87) -> 30;
'yeccgoto_\'Literal\''(95) -> 30;
'yeccgoto_\'Literal\''(106) -> 30.

'yeccgoto_\'LoadNames\''(54) -> 58.

'yeccgoto_\'LoadTag\''(1) -> 4;
'yeccgoto_\'LoadTag\''(113) -> 4;
'yeccgoto_\'LoadTag\''(118) -> 4;
'yeccgoto_\'LoadTag\''(123) -> 4;
'yeccgoto_\'LoadTag\''(128) -> 4;
'yeccgoto_\'LoadTag\''(133) -> 4;
'yeccgoto_\'LoadTag\''(141) -> 4;
'yeccgoto_\'LoadTag\''(144) -> 4;
'yeccgoto_\'LoadTag\''(150) -> 4;
'yeccgoto_\'LoadTag\''(153) -> 4;
'yeccgoto_\'LoadTag\''(159) -> 4.

'yeccgoto_\'NowTag\''(1) -> 3;
'yeccgoto_\'NowTag\''(113) -> 3;
'yeccgoto_\'NowTag\''(118) -> 3;
'yeccgoto_\'NowTag\''(123) -> 3;
'yeccgoto_\'NowTag\''(128) -> 3;
'yeccgoto_\'NowTag\''(133) -> 3;
'yeccgoto_\'NowTag\''(141) -> 3;
'yeccgoto_\'NowTag\''(144) -> 3;
'yeccgoto_\'NowTag\''(150) -> 3;
'yeccgoto_\'NowTag\''(153) -> 3;
'yeccgoto_\'NowTag\''(159) -> 3.

'yeccgoto_\'Value\''(26) -> 29;
'yeccgoto_\'Value\''(46) -> 93;
'yeccgoto_\'Value\''(50) -> 72;
'yeccgoto_\'Value\''(51) -> 68;
'yeccgoto_\'Value\''(52) -> 64;
'yeccgoto_\'Value\''(65) -> 66;
'yeccgoto_\'Value\''(69) -> 70;
'yeccgoto_\'Value\''(74) -> 72;
'yeccgoto_\'Value\''(80) -> 81;
'yeccgoto_\'Value\''(87) -> 89;
'yeccgoto_\'Value\''(95) -> 98;
'yeccgoto_\'Value\''(106) -> 107.

'yeccgoto_\'ValueBraced\''(1) -> 2;
'yeccgoto_\'ValueBraced\''(113) -> 2;
'yeccgoto_\'ValueBraced\''(118) -> 2;
'yeccgoto_\'ValueBraced\''(123) -> 2;
'yeccgoto_\'ValueBraced\''(128) -> 2;
'yeccgoto_\'ValueBraced\''(133) -> 2;
'yeccgoto_\'ValueBraced\''(141) -> 2;
'yeccgoto_\'ValueBraced\''(144) -> 2;
'yeccgoto_\'ValueBraced\''(150) -> 2;
'yeccgoto_\'ValueBraced\''(153) -> 2;
'yeccgoto_\'ValueBraced\''(159) -> 2.

'yeccgoto_\'Variable\''(26) -> 28;
'yeccgoto_\'Variable\''(46) -> 28;
'yeccgoto_\'Variable\''(50) -> 28;
'yeccgoto_\'Variable\''(51) -> 28;
'yeccgoto_\'Variable\''(52) -> 28;
'yeccgoto_\'Variable\''(65) -> 28;
'yeccgoto_\'Variable\''(69) -> 28;
'yeccgoto_\'Variable\''(74) -> 28;
'yeccgoto_\'Variable\''(80) -> 28;
'yeccgoto_\'Variable\''(87) -> 88;
'yeccgoto_\'Variable\''(95) -> 28;
'yeccgoto_\'Variable\''(106) -> 28.

-compile({inline,{yeccpars2_0_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 135).
yeccpars2_0_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_2_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 137).
yeccpars2_2_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_3_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 140).
yeccpars2_3_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_4_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 141).
yeccpars2_4_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_5_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 139).
yeccpars2_5_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_6_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 135).
yeccpars2_6_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_7_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 147).
yeccpars2_7_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 135).
yeccpars2_8_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 146).
yeccpars2_9_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 135).
yeccpars2_10_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 145).
yeccpars2_11_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 135).
yeccpars2_12_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 144).
yeccpars2_13_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 138).
yeccpars2_14_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 142).
yeccpars2_15_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 150).
yeccpars2_16_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 135).
yeccpars2_17_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 149).
yeccpars2_18_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 152).
yeccpars2_19_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 151).
yeccpars2_20_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_21_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 135).
yeccpars2_21_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 143).
yeccpars2_22_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_23_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 135).
yeccpars2_23_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_24_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 148).
yeccpars2_24_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 136).
yeccpars2_27_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 160).
yeccpars2_31_([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{yeccpars2_34_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 154).
yeccpars2_34_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_37_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 156).
yeccpars2_37_([__3,__2,__1 | Stack]) ->
 [begin
   { apply_filter , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 221).
yeccpars2_38_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_40_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 222).
yeccpars2_40_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 , __3 ]
  end | Stack].

-compile({inline,{yeccpars2_41_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 161).
yeccpars2_41_([__3,__2,__1 | Stack]) ->
 [begin
   { attribute , { __3 , __1 } }
  end | Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 229).
yeccpars2_49_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 165).
yeccpars2_57_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { date , now , __3 }
  end | Stack].

-compile({inline,{yeccpars2_59_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 168).
yeccpars2_59_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_60_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 167).
yeccpars2_60_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { load , __3 }
  end | Stack].

-compile({inline,{yeccpars2_61_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 169).
yeccpars2_61_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 164).
yeccpars2_63_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { include , __3 }
  end | Stack].

-compile({inline,{yeccpars2_67_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 213).
yeccpars2_67_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 , __4 ]
  end | Stack].

-compile({inline,{yeccpars2_71_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 207).
yeccpars2_71_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 , __4 ]
  end | Stack].

-compile({inline,{yeccpars2_75_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 199).
yeccpars2_75_([__2,__1 | Stack]) ->
 [begin
   { 'not' , __2 }
  end | Stack].

-compile({inline,{yeccpars2_76_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 198).
yeccpars2_76_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_78_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 227).
yeccpars2_78_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { tag , __2 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_81_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 230).
yeccpars2_81_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __1 ++ [ { __2 , __4 } ]
  end | Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 193).
yeccpars2_84_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_85_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 190).
yeccpars2_85_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_88_close_tag,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 192).
yeccpars2_88_close_tag([__3,__2,__1 | Stack]) ->
 [begin
   { in , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_90_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 194).
yeccpars2_90_([__3,__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __3 ]
  end | Stack].

-compile({inline,{yeccpars2_92_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 163).
yeccpars2_92_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { extends , __3 }
  end | Stack].

-compile({inline,{yeccpars2_93_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 182).
yeccpars2_93_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_96_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 160).
yeccpars2_96_([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{yeccpars2_97_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 185).
yeccpars2_97_([__2,__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_98_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 183).
yeccpars2_98_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_99_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 180).
yeccpars2_99_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { cycle , __3 }
  end | Stack].

-compile({inline,{yeccpars2_100_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 179).
yeccpars2_100_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { cycle_compat , __3 }
  end | Stack].

-compile({inline,{yeccpars2_101_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 187).
yeccpars2_101_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_102_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 186).
yeccpars2_102_([__3,__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_103_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_103_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_105_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 232).
yeccpars2_105_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { call , __3 }
  end | Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 233).
yeccpars2_108_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { call , __3 , __5 }
  end | Stack].

-compile({inline,{yeccpars2_110_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 172).
yeccpars2_110_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_112_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 218).
yeccpars2_112_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_114_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 217).
yeccpars2_114_([__3,__2,__1 | Stack]) ->
 [begin
   { autoescape , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_117_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_117_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_119_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 171).
yeccpars2_119_([__3,__2,__1 | Stack]) ->
 [begin
   { block , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_122_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_122_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_124_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 175).
yeccpars2_124_([__3,__2,__1 | Stack]) ->
 [begin
   { comment , __2 }
  end | Stack].

-compile({inline,{yeccpars2_127_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_127_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_129_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 189).
yeccpars2_129_([__3,__2,__1 | Stack]) ->
 [begin
   { for , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_132_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_132_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_134_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 197).
yeccpars2_134_([__3,__2,__1 | Stack]) ->
 [begin
   { 'if' , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_135_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 135).
yeccpars2_135_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_139_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_139_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_140_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_140_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_142_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 196).
yeccpars2_142_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { ifelse , __1 , __2 , __4 }
  end | Stack].

-compile({inline,{yeccpars2_145_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 206).
yeccpars2_145_([__3,__2,__1 | Stack]) ->
 [begin
   { ifequal , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_146_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 135).
yeccpars2_146_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_149_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_149_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_151_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 205).
yeccpars2_151_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { ifequalelse , __1 , __2 , __4 }
  end | Stack].

-compile({inline,{yeccpars2_154_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 212).
yeccpars2_154_([__3,__2,__1 | Stack]) ->
 [begin
   { ifnotequal , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_155_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 135).
yeccpars2_155_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_158_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_158_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_160_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 211).
yeccpars2_160_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { ifnotequalelse , __1 , __2 , __4 }
  end | Stack].



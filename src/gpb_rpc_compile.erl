-module(gpb_rpc_compile).
-include_lib("gpb/include/gpb.hrl").
%% API
-export([ file/2 ]).

-define(EMPTY_LINE, "\n\n").

file(ProtoFile, GpbRpcOpts) ->
    case file:read_file(ProtoFile) of
        {ok, Binary} ->
            String = binary_to_list(Binary),
            Msg = filename:rootname(filename:basename(ProtoFile)),
            case parse_lines(Msg, String) of
                {ok, Result}->
                    file(Msg, GpbRpcOpts, Result);
                Err -> Err
            end;
        Err -> Err
    end.

file(Msg, GpbRpcOpts, Result) ->
    TargetErlDir = proplists:get_value(o_erl, GpbRpcOpts),
    TargetHrlDir = proplists:get_value(o_hrl, GpbRpcOpts),
    ModuleNameSuffix = proplists:get_value(module_name_suffix, GpbRpcOpts),
    HeaderMsg = proplists:get_value(h_msg, GpbRpcOpts),
    PrefixLen = length(proplists:get_value(msg_prefix, GpbRpcOpts)),
    ModPrefix = proplists:get_value(mod_prefix, GpbRpcOpts),

    ErlTarget = filename:join([TargetErlDir, Msg ++ ".erl"]),
    HrlTarget = filename:join([TargetHrlDir, Msg ++ ".hrl"]),

    gen_mod(Msg, ModuleNameSuffix, PrefixLen, ModPrefix, HeaderMsg, ErlTarget, Result),
    gen_hrl(Msg, HrlTarget, Result),
    ok.

parse_lines(FName, String) ->
    case gpb_scan:string(String) of
        {ok, Tokens, _} ->
            case gpb_parse:parse(Tokens++[{'$end',99999}]) of
                {ok, Result} ->
                    {ok, Result};
                {error, {LNum,_Module,EMsg}=Reason} ->
                    io:format("Parse error on line ~w:~n  ~p~n",
                        [LNum, {Tokens,EMsg}]),
                    {error, {parse_error, FName, Reason}}
            end;
        {error, Reason} ->
            io:format("Scan error:~n  ~p~n", [Reason]),
            erlang:error({scan_error, FName, Reason})
    end.

gen_mod(Msg, ModuleNameSuffix, PrefixLen, ModPrefix, HeaderMsg, Target, Result) ->
    Service = list_to_atom(Msg++"_service"),
    case lists:keyfind({service, Service}, 1, Result) of
        {_, RpcList} ->
            {CallbackList, Body} = gen_rpc_list(Msg, ModuleNameSuffix, PrefixLen, ModPrefix, HeaderMsg, RpcList),
            MsgPb = Msg++ModuleNameSuffix,
            IoData = [
                gen_header(Msg, MsgPb), ?EMPTY_LINE,
                CallbackList, ?EMPTY_LINE,
                Body, ?EMPTY_LINE,
                gen_encode_msg(MsgPb, HeaderMsg), ?EMPTY_LINE,
                gen_send_msg(), ?EMPTY_LINE,
                gen_footer()
            ],
            file:write_file(Target, IoData);
        false -> none
    end.

gen_hrl(Msg0, Target, Result) ->
    Msg = string:to_upper(Msg0),
    Body = [ gen_macros(Prefix, EnumList) || {{enum, Prefix}, EnumList} <- Result],
    IoData = [gen_hrl_header(Msg), Body, gen_hrl_footer()],
    file:write_file(Target, IoData).

gen_hrl_header(Msg) ->
"-ifndef("++Msg++"_H).
-define("++Msg++"_H, true).\n".

gen_macros(Prefix, EnumList) ->
    [gen_macro(Prefix, Key, Value)||{Key, Value} <- EnumList].
gen_macro(Prefix, Key, Value) ->
    UpperPrefix = string:to_upper(atom_to_list(Prefix)),
    UpperKey = string:to_upper(atom_to_list(Key)),
    Macro = UpperPrefix++"_"++UpperKey,
    MacroKey = UpperPrefix++"_"++UpperKey++"_KEY",
    [
        io_lib:format("-define(~ts, ~p).%~p~n", [Macro, Value, Key]),
        io_lib:format("-define(~ts, '~p').%~p~n", [MacroKey, Key, Value])
    ].

gen_hrl_footer() ->
    "\n-endif.".

gen_header(Msg, MsgPb) ->
    UpperMsg = string:to_upper(Msg),
"-module("++ Msg ++").

-include(\"msg.hrl\").
-ifdef(CMD_"++UpperMsg++").
-include(\""++Msg++".hrl\").
-include(\""++MsgPb++".hrl\").

-define(THIS_CMD, ?CMD_"++UpperMsg++").

-export([
    handle_msg/3,
    decode_input_msg/1,
    decode_output_msg/1,
    decode_input/2,
    decode_output/2,
    encode_msg/2,
    send_msg/3
]).".

gen_rpc_list(Msg, ModuleNameSuffix, PrefixLen, ModPrefix, HeaderMsg, RpcList) ->
    Mod = ModPrefix++string:substr(Msg, PrefixLen+1),
    MsgPb = Msg++ModuleNameSuffix,
    [CallbackList, HandleMsg, DecodeInput, DecodeOutput] =
        lists:foldl(
            fun(Rpc, Acc) ->
                GenList = gen_rpc(MsgPb, HeaderMsg, Mod, Rpc),
                lists:zipwith(fun(Gen, OldList) -> [Gen|OldList] end, Acc, GenList)
            end, lists:duplicate(4, []), RpcList),
    BodyList =
    [
        HandleMsg,
        gen_handle_msg_last(),?EMPTY_LINE,
        gen_decode_input_msg(),?EMPTY_LINE,
        gen_decode_output_msg(),?EMPTY_LINE,
        DecodeInput,
        gen_decode_input_last(),?EMPTY_LINE,
        DecodeOutput,
        gen_decode_output_last(),?EMPTY_LINE
    ],
    {CallbackList,BodyList}.

gen_rpc(MsgPb, HeaderMsg, Mod, {Func0, {[Input0], _}, {[Output0], _}, _}) ->
    Func = atom_to_list(Func0),
    Input = atom_to_list(Input0),
    UpperInput = string:to_upper(Input),
    Output = atom_to_list(Output0),
    UpperOutput = string:to_upper(Output),
    [
        gen_callback(Func, HeaderMsg),
        gen_handle_msg(Mod, Func, MsgPb, Input, UpperInput, UpperOutput),
        gen_decode_input(Mod, Func, MsgPb, Input, UpperInput),
        gen_decode_output(MsgPb, Output, UpperOutput)
    ].

gen_callback(Func, HeaderMsg) ->
"-callback "++Func++"(Msg :: "++HeaderMsg++":msg(), State :: "++HeaderMsg++":state()) -> Reply :: "++HeaderMsg++":func_reply().\n".

gen_handle_msg(_Mod, _Func, _MsgPb, _Input, "UNDEFINED", _UpperOutput) -> "";
gen_handle_msg(Mod, Func, MsgPb, Input, UpperInput, "UNDEFINED") ->
"handle_msg(?C_CMD_"++UpperInput++", Binary, State) ->
    Msg = "++MsgPb++":decode_msg(Binary, "++Input++"),
    "++Mod++":"++Func++"(Msg, State);\n";
gen_handle_msg(Mod, Func, MsgPb, Input, UpperInput, UpperOutput) ->
"handle_msg(?C_CMD_"++UpperInput++", Binary, State) ->
    Msg = "++MsgPb++":decode_msg(Binary, "++Input++"),
    Maps = "++Mod++":"++Func++"(Msg, State),
    case maps:find(msg, Maps) of
        {ok, RespMsg} ->
            PbBinary = "++MsgPb++":encode_msg(RespMsg),
            RespBinary = bg_msg:encode_msg(?THIS_CMD, ?C_CMD_"++UpperOutput++", PbBinary),
            Maps#{resp_binary => RespBinary};
        error -> Maps
    end;\n".
gen_handle_msg_last() ->
"handle_msg(CCmd, _Binary, _State) ->
    #{error => {not_defined_c_cmd, CCmd}}.".

gen_decode_input_msg() ->
"decode_input_msg(Binary) ->
    {?THIS_CMD, CCmd, MsgBinary} = bg_msg:decode_msg(Binary),
    decode_input(CCmd, MsgBinary).".

gen_decode_output_msg() ->
"decode_output_msg(Binary) ->
    {?THIS_CMD, CCmd, MsgBinary} = bg_msg:decode_msg(Binary),
    decode_output(CCmd, MsgBinary).".

gen_decode_input(_Mod, _Func, _MsgPb, _Input, "UNDEFINED") -> "";
gen_decode_input(Mod, Func, MsgPb, Input, UpperInput) ->
"decode_input(?C_CMD_"++UpperInput++", MsgBinary) ->
    {"++Mod++", "++Func++", "++MsgPb++":decode_msg(MsgBinary, "++Input++")};\n".
gen_decode_input_last() ->
"decode_input(CCmd, _MsgBinary) ->
    {error, {not_defined_c_cmd, CCmd}}.".

gen_decode_output(_MsgPb, _Output, "UNDEFINED") -> "";
gen_decode_output(MsgPb, Output, UpperOutput) ->
"decode_output(?C_CMD_"++UpperOutput++", MsgBinary) ->
    "++MsgPb++":decode_msg(MsgBinary, "++Output++");\n".
gen_decode_output_last() ->
"decode_output(CCmd, _MsgBinary) ->
    {error, {not_defined_c_cmd, CCmd}}.".

gen_encode_msg(MsgPb, HeaderMsg) ->
"encode_msg(CCmd, RespMsg) ->
    RespBinary = "++MsgPb++":encode_msg(RespMsg),
    "++HeaderMsg++":encode_msg(?THIS_CMD, CCmd, RespBinary).".

gen_footer() ->
    "-endif.".

gen_send_msg() ->
"send_msg(undefined, _CCmd, _Msg) -> undefined;
send_msg(Pid, CCmd, Msg) ->
        Binary = encode_msg(CCmd, Msg),
        Pid ! {send, Binary},
        ok.".

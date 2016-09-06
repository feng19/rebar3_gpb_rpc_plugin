-module(gpb_rpc_compile).
-include_lib("gpb/include/gpb.hrl").
%% API
-export([ file/2 ]).

file(ProtoFile, GpbRpcOpts) ->
    String = binary_to_list(element(2, file:read_file(ProtoFile))),
    Msg = filename:rootname(filename:basename(ProtoFile)),
    case parse_lines(Msg, String) of
        {ok, Result}->
            TargetErlDir = proplists:get_value(o_erl, GpbRpcOpts),
            TargetHrlDir = proplists:get_value(o_hrl, GpbRpcOpts),
            ModuleNameSuffix = proplists:get_value(module_name_suffix, GpbRpcOpts),
            HeaderMsg = proplists:get_value(h_msg, GpbRpcOpts),

            ErlTarget = filename:join([TargetErlDir, Msg ++ ".erl"]),
            HrlTarget = filename:join([TargetHrlDir, Msg ++ ".hrl"]),

            gen_mod(Msg, ModuleNameSuffix, HeaderMsg, ErlTarget, Result),
            gen_hrl(HrlTarget, Result),
            ok;
        Err -> Err
    end.

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

gen_mod(Msg, ModuleNameSuffix, HeaderMsg, Target, Result) ->
    Service = list_to_atom(Msg++"_service"),
    case lists:keyfind({service, Service}, 1, Result) of
        {_, RpcList} ->
            {CallbackList, Body} = gen_rpc_list(Msg, ModuleNameSuffix, HeaderMsg, RpcList),
            IoData = [
                gen_header(Msg, ModuleNameSuffix),
                CallbackList, "\n",
                gen_handle_msg(Msg, ModuleNameSuffix),
                Body,
                gen_encode_msg(Msg, ModuleNameSuffix, HeaderMsg)
            ],
            file:write_file(Target, IoData);
        false -> none
    end.

gen_hrl(Target, Result) ->
    IoData = [ gen_macros(Prefix, EnumList) || {{enum, Prefix}, EnumList} <- Result],
    file:write_file(Target, IoData).

gen_macros(Prefix, EnumList) ->
    [gen_macro(Prefix, Key, Value)||{Key, Value} <- EnumList].
gen_macro(Prefix, Key, Value) ->
    Macro = string:to_upper(atom_to_list(Prefix))++"_"++string:to_upper(atom_to_list(Key)),
    io_lib:format("-define(~ts, ~p).%~p~n", [Macro, Key, Value]).

gen_header(Msg, ModuleNameSuffix) ->
"-module("++ Msg ++").
-include(\""++ Msg ++ModuleNameSuffix++".hrl\").

-export([
    handle_msg/2,
    decode_input_msg/1,
    decode_output_msg/1,
    decode_input/2,
    decode_output/2,
    encode_msg/2
]).\n\n".

gen_handle_msg(Msg, ModuleNameSuffix) ->
"handle_msg(Binary, State) ->
        #"++ Msg ++"{func = Func, pb_msg = Request} = "++ Msg ++ModuleNameSuffix++":decode_msg(Binary, "++ Msg ++"),
    case handle_msg(Func, Request, State) of
        {reply_msg, RespMsg} ->
            {reply, encode_msg(Func, RespMsg)};
        {reply_msg, RespMsg, NewState} ->
            {reply, encode_msg(Func, RespMsg), NewState};
        {stop_msg, RespMsg} ->
            {stop, encode_msg(Func, RespMsg)};
        {stop_msg, RespMsg, NewState} ->
            {stop, encode_msg(Func, RespMsg), NewState};
        Reply -> Reply
    end.\n\n".

gen_rpc_list(Msg, ModuleNameSuffix, HeaderMsg, RpcList) ->
    [$m,$s,$g,$_|Mod] = Msg,
    [CallbackList|BodyList] =
        lists:foldl(
            fun(Rpc, Acc) ->
                GenList = gen_rpc(Msg, ModuleNameSuffix, HeaderMsg, Mod, Rpc),
                lists:zipwith(fun(Gen, OldList) -> [Gen|OldList] end, Acc, GenList)
            end, lists:duplicate(6, []), RpcList),
    {
        CallbackList,
        [ [lists:reverse(tl(lists:reverse(Body))),".\n\n"] || Body <- BodyList ]
    }.

gen_rpc(Msg, ModuleNameSuffix, HeaderMsg, Mod, #rpc{name = Func0, input = Input0, output = Output0}) ->
    Func = atom_to_list(Func0),
    Input = atom_to_list(Input0),
    Output = atom_to_list(Output0),
    [
        gen_callback(Func, HeaderMsg),
        gen_handle_msg(Func, Mod, Msg, ModuleNameSuffix, Input),
        gen_decode_input_msg(Msg, ModuleNameSuffix),
        gen_decode_output_msg(Msg, ModuleNameSuffix),
        gen_decode_input(Func, Mod, Msg, ModuleNameSuffix, Input),
        gen_decode_output(Func, Msg, ModuleNameSuffix, Output)
    ].

gen_callback(Func, HeaderMsg) ->
"-callback "++Func++"(Msg :: "++HeaderMsg++":msg(), State :: "++HeaderMsg++":state()) -> Reply :: "++HeaderMsg++":func_reply().\n".

gen_handle_msg(Func, Mod, Msg, ModuleNameSuffix, Input) ->
"handle_msg("++Func++", Binary, State) ->
    mod_"++Mod++":"++Func++"("++Msg++ModuleNameSuffix++":decode_msg(Binary, "++Input++"), State);".

gen_decode_input_msg(Msg, ModuleNameSuffix) ->
"decode_input_msg(Binary) ->
        #"++Msg++"{func = Func, pb_msg = Request} = "++Msg++ModuleNameSuffix++":decode_msg(Binary, "++Msg++"),
    decode_input(Func, Request).".

gen_decode_output_msg(Msg, ModuleNameSuffix) ->
"decode_output_msg(Binary) ->
        #"++Msg++"{func = Func, pb_msg = Request} = "++Msg++ModuleNameSuffix++":decode_msg(Binary, "++Msg++"),
    decode_output(Func, Request).".

gen_decode_input(Func, Mod, Msg, ModuleNameSuffix, Input) ->
"decode_input("++Func++", Binary) ->
    {mod_"++Mod++", "++Func++", "++Msg++ModuleNameSuffix++":decode_msg(Binary, "++Input++")};".

gen_decode_output(Func, Msg, ModuleNameSuffix, Output) ->
"decode_output("++Func++", Binary) ->
    "++Msg++ModuleNameSuffix++":decode_msg(Binary, "++Output++");".

gen_encode_msg(Msg, ModuleNameSuffix, HeaderMsg) ->
"encode_msg(Func, RespMsg) ->
        RespBinary = encode_func_msg(Func, RespMsg),
        "++HeaderMsg++":encode_msg(?MODULE, RespBinary).
    encode_func_msg(Func, RespMsg) ->
        Binary = "++Msg++ModuleNameSuffix++":encode_msg(RespMsg),
    "++Msg++ModuleNameSuffix++":encode_msg(#"++Msg++"{func = Func, pb_msg = Binary}).".
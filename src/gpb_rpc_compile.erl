-module(gpb_rpc_compile).
-include_lib("gpb/include/gpb.hrl").
%% API
-export([ file/4 ]).

-define(EMPTY_LINE, "\n\n").

file(ProtoFile, ErlTplFile, HrlTplFile, GpbRpcOpts) ->
    case file:read_file(ProtoFile) of
        {ok, Binary} ->
            String = binary_to_list(Binary),
            ProtoName = filename:rootname(filename:basename(ProtoFile)),
            case parse_lines(ProtoName, String) of
                {ok, ScanProtoResult}->
                    file(ProtoName, ErlTplFile, HrlTplFile, GpbRpcOpts, ScanProtoResult);
                Err -> Err
            end;
        Err -> Err
    end.

file(ProtoName, ErlTplFile, HrlTplFile, GpbRpcOpts, ScanProtoResult) ->
    TargetErlDir = proplists:get_value(o_erl, GpbRpcOpts),
    TargetHrlDir = proplists:get_value(o_hrl, GpbRpcOpts),
    ModuleNameSuffix = proplists:get_value(module_name_suffix, GpbRpcOpts),
    PrefixLen = length(proplists:get_value(msg_prefix, GpbRpcOpts)),
    ModPrefix = proplists:get_value(mod_prefix, GpbRpcOpts),

    ErlTarget = filename:join([TargetErlDir, ProtoName ++ ".erl"]),
    HrlTarget = filename:join([TargetHrlDir, ProtoName ++ ".hrl"]),

    gen_mod(ProtoName, ErlTplFile, ModuleNameSuffix, PrefixLen, ModPrefix, ErlTarget, ScanProtoResult),
    gen_hrl(ProtoName, HrlTplFile, HrlTarget, ScanProtoResult),
    ok.

parse_lines(ProtoName, String) ->
    case gpb_scan:string(String) of
        {ok, Tokens, _} ->
            case gpb_parse:parse(Tokens ++ [{'$end', 99999}]) of
                {ok, Result} ->
                    {ok, Result};
                {error, {LNum, _Module, EMsg} = Reason} ->
                    rebar_api:abort("Parse error on line ~w:~n~p", [LNum, {Tokens, EMsg}]),
                    {error, {parse_error, ProtoName, Reason}}
            end;
        {error, Reason} ->
            rebar_api:abort("Scan error:~n~p", [Reason]),
            erlang:error({scan_error, ProtoName, Reason})
    end.

%%--------------------------------------------------------------------
gen_mod(ProtoName, ErlTplFile, ModuleNameSuffix, PrefixLen, ModPrefix, Target, ScanProtoResult) ->
    case file:read_file(ErlTplFile) of
        {ok, RenderBin} ->
            Service = list_to_atom(ProtoName ++"_service"),
            case lists:keyfind({service, Service}, 1, ScanProtoResult) of
                {_, RpcList} ->
                    RenderData = gen_mod_do(ProtoName, ModuleNameSuffix, PrefixLen, ModPrefix, RpcList),
                    IoData = bbmustache:render(RenderBin, RenderData, [{key_type, atom}]),
                    file:write_file(Target, IoData);
                false ->
                    rebar_api:debug("skipped gen gpb rpc : ~p", [ProtoName])
            end;
        {error, Reason} ->
            rebar_api:abort("mustache read file:~p ~p", [ErlTplFile, Reason])
    end.
gen_mod_do(ProtoName, ModuleNameSuffix, PrefixLen, ModPrefix, RpcList0) ->
    GpbProto = ProtoName ++ ModuleNameSuffix,
    HandleMod = ModPrefix++string:substr(ProtoName, PrefixLen+1),
    ProtoNameUpper = string:to_upper(ProtoName),
    BaseData = [
        {proto_name, ProtoName},
        {proto_name_upper, ProtoNameUpper},
        {gpb_proto, GpbProto},
        {handle_mod, HandleMod}
    ],
    {CallbackList, RpcList, InputList, OutputList, _} =
        lists:foldl(
            fun(Rpc, Acc) ->
                erl_rpc(Rpc, Acc, BaseData)
            end, {[], [], [], [], []}, RpcList0),
    [
        {proto_name, ProtoName},
        {proto_name_upper, ProtoNameUpper},
        {gpb_proto, GpbProto},
        {handle_mod, HandleMod},
        {callback_list, CallbackList},
        {rpc_list, RpcList},
        {input_list, InputList},
        {output_list, OutputList}|BaseData
    ].

erl_rpc({Func0, {[undefined], _}, {[Output0], _}, _}, {CallbackList, RpcList, InputList, OutputList, OutputListAcc}, BaseData) ->
    Func = atom_to_list(Func0),
    Output = atom_to_list(Output0),
    OutputUpper = string:to_upper(Output),
    RpcData = [
        {output, Output},{output_upper, OutputUpper},
        {handle_func, Func}|BaseData
    ],
    case lists:member(Output0, OutputListAcc) of
        true ->
            {
                CallbackList,
                RpcList,
                InputList,
                OutputList,
                OutputListAcc
            };
        false ->
            {
                CallbackList,
                RpcList,
                InputList,
                [RpcData|OutputList],
                [Output0|OutputListAcc]
            }
    end;
erl_rpc({Func0, {[Input0], _}, {[undefined], _}, _}, {CallbackList, RpcList, InputList, OutputList, OutputListAcc}, BaseData) ->
    Func = atom_to_list(Func0),
    Input = atom_to_list(Input0),
    InputUpper = string:to_upper(Input),
    NewCallbackList = [{handle_func, Func}|CallbackList],
    RpcData = [
        {input, Input},{input_upper, InputUpper},
        {handle_func, Func}|BaseData
    ],
    {
        NewCallbackList,
        RpcList,
        [RpcData|InputList],
        OutputList,
        OutputListAcc
    };
erl_rpc({Func0, {[Input0], _}, {[Output0], _}, _}, {CallbackList, RpcList, InputList, OutputList, OutputListAcc}, BaseData) ->
    Func = atom_to_list(Func0),
    Input = atom_to_list(Input0),
    InputUpper = string:to_upper(Input),
    Output = atom_to_list(Output0),
    OutputUpper = string:to_upper(Output),
    HandleFunc = [{handle_func, Func}],
    NewCallbackList = [HandleFunc|CallbackList],
    RpcData = [
        {input, Input},{input_upper, InputUpper},
        {output, Output},{output_upper, OutputUpper},
        {handle_func, Func}|BaseData
    ],
    case lists:member(Output0, OutputListAcc) of
        true ->
            {
                NewCallbackList,
                [RpcData|RpcList],
                InputList,
                OutputList,
                OutputListAcc
            };
        false ->
            {
                NewCallbackList,
                [RpcData|RpcList],
                InputList,
                [RpcData|OutputList],
                [Output0|OutputListAcc]
            }
    end.

%%--------------------------------------------------------------------
gen_hrl(ProtoName, HrlTplFile, Target, Result) ->
    case file:read_file(HrlTplFile) of
        {ok, RenderBin} ->
            ProtoNameUpper = string:to_upper(ProtoName),
            BaseData = [
                {proto_name, ProtoName},
                {proto_name_upper, ProtoNameUpper}
            ],
            EnumsList = [ hrl_enums_list(EnumName, EnumList, BaseData) || {{enum, EnumName}, EnumList} <- Result],
            RenderData = [{enums_list, EnumsList}|BaseData],
            IoData = bbmustache:render(RenderBin, RenderData, [{key_type, atom}]),
            file:write_file(Target, IoData);
        {error, Reason} ->
            rebar_api:abort("mustache read file:~p ~p", [HrlTplFile, Reason])
    end.
hrl_enums_list(EnumName, EnumList0, BaseData0) ->
    EnumNameUpper = string:to_upper(atom_to_list(EnumName)),
    BaseData = [
        {enum_name, EnumName},
        {enum_name_upper, EnumNameUpper}|BaseData0
    ],
    EnumList = [hrl_enum(EnumKey, EnumValue, BaseData)||{EnumKey, EnumValue} <- EnumList0],
    [
        {enum_list, EnumList}|BaseData
    ].
hrl_enum(EnumKey, EnumValue, BaseData) ->
    EnumKeyUpper = string:to_upper(atom_to_list(EnumKey)),
    [
        {enum_key, EnumKey},
        {enum_key_upper, EnumKeyUpper},
        {enum_value, EnumValue}|BaseData
    ].
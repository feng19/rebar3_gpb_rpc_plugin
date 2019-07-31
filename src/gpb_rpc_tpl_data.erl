-module(gpb_rpc_tpl_data).
-include_lib("gpb/include/gpb.hrl").

%% API
-export([
    router_mod_data/5,
    mod_file_data/5,
    hrl_file_data/4
]).

-define(MSG_DATA(Cmd, CCmd, Msg, MsgUpper, IsEmpty, Func, BaseData),
    [
        {cmd, Cmd}, {ccmd, CCmd},
        {msg, Msg}, {msg_upper, MsgUpper},
        {is_empty, IsEmpty}, {handle_func, Func} | BaseData
    ]).

%%--------------------------------------------------------------------
%% router_mod_data
%%--------------------------------------------------------------------
router_mod_data(AppDir, RouterFile, GpbRpcOpts, GpbOpts, RouterDefines) ->
    rebar_api:debug("RouterFile:~ts, Defines:~n~p", [RouterFile, RouterDefines]),
    % fist enum is cmd defines
    {_, RouterCmds} = hd([{EnumName, EnumList} || {{enum, EnumName}, EnumList} <- RouterDefines]),
    SourceDirs = proplists:lookup_all(i, GpbRpcOpts),
    RpcModuleNameSuffix = proplists:get_value(module_name_suffix, GpbRpcOpts),
    RouterFile0 = filename:basename(RouterFile, ".proto") ++ RpcModuleNameSuffix,
    PrefixLen = length(proplists:get_value(msg_prefix, GpbRpcOpts)),
    ModPrefix = proplists:get_value(mod_prefix, GpbRpcOpts),
    CCmdBit = proplists:get_value(ccmd_bit, GpbRpcOpts),
    % GenMode = proplists:get_value(gen_mode, GpbRpcOpts, server),
    ModuleNameSuffix = proplists:get_value(module_name_suffix, GpbOpts),
    IsSnakeCase = proplists:is_defined(msg_name_to_snake_case, GpbOpts),

    #{req_list := ReqList0, resp_list := RespList0, router_cmd_list := RouterCmdList} = lists:foldl(
        fun({CmdName, Cmd}, #{router_cmd_list := RouterCmdListT} = Acc) ->
            ProtoName = atom_to_list(CmdName),
            case gpb_rpc_compile:find_proto_file(AppDir, ProtoName, SourceDirs, GpbOpts) of
                [{_, Defines}] ->
                    rebar_api:debug("ProtoFile:~ts.proto, Defines:~n~p", [ProtoName, Defines]),
                    RouterCmdTerm = [{cmd_name, CmdName}, {cmd, Cmd}],
                    router_mod_data_do(ProtoName, Cmd, Defines, RpcModuleNameSuffix, ModuleNameSuffix,
                        PrefixLen, ModPrefix, CCmdBit, IsSnakeCase,
                        Acc#{router_cmd_list => [RouterCmdTerm | RouterCmdListT]});
                [] -> % didn't found
                    rebar_utils:abort("msg module: ~p define in router_file:~ts.proto, but didn't found ~p.proto file",
                        [CmdName, RouterFile0, CmdName]);
                L when is_list(L) ->
                    rebar_utils:abort("duplicate file: ~p.proto", [CmdName])
            end
        end, #{req_list => [], resp_list => [], router_cmd_list => []}, RouterCmds),

    ReqList = lists:reverse(ReqList0),
    RespList = lists:reverse(RespList0),
    CmdList = add_is_last(lists:sort(ReqList ++ RespList)),

    ErlRenderData = [
        {file, RouterFile0},
        {file_upper, string:to_upper(RouterFile0)},
        {req_list, add_is_last(ReqList)},
        {resp_list, add_is_last(RespList)},
        {router_cmd_list, add_is_last(lists:reverse(RouterCmdList))},
        {cmd_list, CmdList}
    ],

    HrlRenderData = [
        {file, RouterFile0},
        {file_upper, string:to_upper(RouterFile0)},
        {cmd_list, CmdList},
        {hrl_list, [[{hrl_name, lists:concat([ModCmd, ModuleNameSuffix])}] || {ModCmd, _} <- RouterCmds]}
    ],

    {RouterFile0, ErlRenderData, HrlRenderData}.

router_mod_data_do(ProtoName, RouterCmd, Defines, RpcModuleNameSuffix, ModuleNameSuffix,
    PrefixLen, ModPrefix, CCmdBit, IsSnakeCase, Acc0) ->
    Service = list_to_atom(ProtoName ++ "_service"),
    RpcList =
        case lists:keyfind({service, Service}, 1, Defines) of
            {_, RpcListT} -> RpcListT;
            _ ->
                rebar_utils:abort("Didn't found ~p in ~ts.proto file", [Service, ProtoName])
        end,
    % fist enum is ccmd defines
    {_, CCmdList0} = hd([{EnumName, EnumList} || {{enum, EnumName}, EnumList} <- Defines]),
    GpbProto = ProtoName ++ ModuleNameSuffix,
    HandleMod = ModPrefix ++ string:substr(ProtoName, PrefixLen + 1) ++ RpcModuleNameSuffix,
    ProtoNameUpper = string:to_upper(ProtoName),
    BaseData = [
        {mod_cmd, RouterCmd},
        {mod_cmd_name, ProtoName},
        {mod_cmd_name_upper, ProtoNameUpper},
        {gpb_proto, GpbProto},
        {handle_mod, HandleMod}
    ],
    CCmdList = [
        begin
            NewCCmdName =
                case IsSnakeCase of
                    true ->
                        list_to_atom(gpb_lib:snake_case(atom_to_list(CCmdName)));
                    _ -> CCmdName
                end,
            {NewCCmdName, {CCmd, RouterCmd bsl CCmdBit + CCmd}}
        end || {CCmdName, CCmd} <- CCmdList0],

    lists:foldl(
        fun(Rpc, Acc) ->
            erl_rpc(Rpc, CCmdList, BaseData, ProtoName, Defines, Acc)
        end, Acc0#{resp_names => []}, RpcList).

%% Empty for proto3; undefined for proto2
erl_rpc(#?gpb_rpc{name = Func0, input = Input, output = Output0}, CCmdList, BaseData, ProtoName, Defines,
    #{resp_names := RespNames} = Acc) when (Input =:= 'Empty' orelse Input =:= empty orelse Input =:= undefined)
    andalso Output0 =/= 'Empty' andalso Output0 =/= empty andalso Output0 =/= undefined ->
    Func = atom_to_list(Func0),
    Output = atom_to_list(Output0),
    case lists:member(Output0, RespNames) of
        true -> Acc;
        false ->
            #{resp_list := RespList, resp_names := RespNames} = Acc,
            {CCmd, Cmd} =
                case proplists:get_value(Output0, CCmdList) of
                    undefined ->
                        rebar_utils:abort("Didn't define c_cmd: ~p in ~ts.proto file", [Output0, ProtoName]);
                    R -> R
                end,
            OutputUpper = string:to_upper(Output),
            IsEmpty =
                case lists:keyfind({msg, Output0}, 1, Defines) of
                    false ->
                        rebar_utils:abort("Didn't define msg: ~p in ~ts.proto file", [Output0, ProtoName]);
                    {_, Fields} -> Fields == []
                end,
            RespData = ?MSG_DATA(Cmd, CCmd, Output, OutputUpper, IsEmpty, Func, BaseData),
            Acc#{
                resp_list => [RespData | RespList],
                resp_names => [Output0 | RespNames]
            }
    end;
erl_rpc(#?gpb_rpc{name = Func0, input = Input0, output = Output}, CCmdList, BaseData, ProtoName, Defines,
    #{req_list := ReqList} = Acc) when (Output =:= 'Empty' orelse Output =:= empty orelse Output =:= undefined)
    andalso Input0 =/= 'Empty' andalso Input0 =/= empty andalso Input0 =/= undefined ->
    Func = atom_to_list(Func0),
    Input = atom_to_list(Input0),
    InputUpper = string:to_upper(Input),
    {CCmd, Cmd} =
        case proplists:get_value(Input0, CCmdList) of
            undefined ->
                rebar_utils:abort("Didn't define c_cmd: ~p in ~ts.proto file", [Input0, ProtoName]);
            R -> R
        end,
    IsEmpty =
        case lists:keyfind({msg, Input0}, 1, Defines) of
            false ->
                rebar_utils:abort("Didn't define msg: ~p in ~ts.proto file", [Input0, ProtoName]);
            {_, Fields} -> Fields == []
        end,
    ReqData = ?MSG_DATA(Cmd, CCmd, Input, InputUpper, IsEmpty, Func, BaseData),
    Acc#{
        req_list => [ReqData | ReqList]
    };
erl_rpc(#?gpb_rpc{name = Func0, input = Input0, output = Output0}, CCmdList, BaseData, ProtoName, Defines,
    #{req_list := ReqList, resp_list := RespList, resp_names := RespNames} = Acc)
    when Input0 =/= 'Empty' andalso Input0 =/= empty andalso Input0 =/= undefined
    andalso Output0 =/= 'Empty' andalso Output0 =/= empty andalso Output0 =/= undefined ->
    Func = atom_to_list(Func0),
    Input = atom_to_list(Input0),
    InputUpper = string:to_upper(Input),
    Output = atom_to_list(Output0),
    OutputUpper = string:to_upper(Output),
    {InputCCmd, InputCmd} =
        case proplists:get_value(Input0, CCmdList) of
            undefined ->
                rebar_utils:abort("Didn't define c_cmd: ~p in ~ts.proto file", [Input0, ProtoName]);
            InR -> InR
        end,
    InIsEmpty =
        case lists:keyfind({msg, Input0}, 1, Defines) of
            false ->
                rebar_utils:abort("Didn't define msg: ~p in ~ts.proto file", [Input0, ProtoName]);
            {_, InFields} -> InFields == []
        end,
    ReqData = ?MSG_DATA(InputCmd, InputCCmd, Input, InputUpper, InIsEmpty, Func, BaseData),
    case lists:member(Output0, RespNames) of
        true -> % already have resp
            Acc#{
                req_list => [ReqData | ReqList],
                resp_list=> RespList,
                resp_names => RespNames
            };
        false ->
            {OutputCCmd, OutputCmd} =
                case proplists:get_value(Output0, CCmdList) of
                    undefined ->
                        rebar_utils:abort("Didn't define c_cmd: ~p in ~ts.proto file", [Output0, ProtoName]);
                    OutR -> OutR
                end,
            OutIsEmpty =
                case lists:keyfind({msg, Output0}, 1, Defines) of
                    false ->
                        rebar_utils:abort("Didn't define msg: ~p in ~ts.proto file", [Output0, ProtoName]);
                    {_, OutFields} -> OutFields == []
                end,
            RespData = ?MSG_DATA(OutputCmd, OutputCCmd, Output, OutputUpper, OutIsEmpty, Func, BaseData),
            Acc#{
                req_list => [ReqData | ReqList],
                resp_list => [RespData | RespList],
                resp_names => [Output0 | RespNames]
            }
    end.

%%--------------------------------------------------------------------
%% mod_file_data
%%--------------------------------------------------------------------
mod_file_data(FileName, ProtoName, GpbProto, Defines, GenMode) ->
    Service = list_to_atom(ProtoName ++ "_service"),
    case lists:keyfind({service, Service}, 1, Defines) of
        {_, RpcList} ->
            CallbackList = get_callback_list(GenMode, GpbProto, RpcList),
            case CallbackList of
                [] -> skip;
                _ ->
                    [
                        {file, FileName},
                        {gpb_proto, GpbProto},
                        {callback_list, CallbackList}
                    ]
            end;
        false -> skip
    end.

get_callback_list(server, GpbProto, RpcList) ->
    [begin
         Func = atom_to_list(Func0),
         Input = atom_to_list(Input0),
         [
             {gpb_proto, GpbProto},
             {callback, Func},
             {req, Input}
         ]
     end || #?gpb_rpc{input = Input0, name = Func0} <- RpcList,
        Input0 =/= 'Empty' andalso Input0 =/= empty andalso Input0 =/= undefined];
get_callback_list(_, GpbProto, RpcList0) -> % client
    RpcList = lists:foldl(
        fun(#?gpb_rpc{output = Output} = Rpc, Acc) ->
            case lists:keymember(Output, #?gpb_rpc.output, Acc) of
                true -> Acc;
                _ -> [Rpc | Acc]
            end
        end, [], RpcList0),
    [begin
         Func = atom_to_list(Func0),
         Output = atom_to_list(Output0),
         [
             {gpb_proto, GpbProto},
             {callback, Func},
             {resp, Output}
         ]
     end || #?gpb_rpc{output = Output0, name = Func0} <- lists:reverse(RpcList),
        Output0 =/= 'Empty' andalso Output0 =/= empty andalso Output0 =/= undefined].

%%--------------------------------------------------------------------
%% hrl_file_data
%%--------------------------------------------------------------------
hrl_file_data(FileName, ProtoName, GpbProto, Defines) ->
    FileNameUpper = string:to_upper(FileName),
    BaseData = [
        {file, FileName},
        {file_upper, FileNameUpper},
        {gpb_proto, GpbProto}
    ],
    Service = list_to_atom(ProtoName ++ "_service"),
    AllEnumList0 = [{EnumName, EnumList} || {{enum, EnumName}, EnumList} <- Defines],

    AllEnumList =
        case lists:keymember({service, Service}, 1, Defines) of
            true ->
                tl(AllEnumList0);
            _ -> AllEnumList0
        end,
    case AllEnumList of
        [] -> skip;
        _ ->
            EnumsList = [hrl_enums_list(EnumName, EnumList, BaseData)
                || {EnumName, EnumList} <- AllEnumList],
            [{enums_list, EnumsList} | BaseData]
    end.
hrl_enums_list(EnumName, EnumList0, BaseData0) ->
    EnumNameUpper = string:to_upper(atom_to_list(EnumName)),
    BaseData = [
        {enum_name, EnumName},
        {enum_name_upper, EnumNameUpper} | BaseData0
    ],
    EnumList = [hrl_enum(EnumKey, EnumValue, BaseData) || {EnumKey, EnumValue} <- EnumList0],
    [
        {enum_list, EnumList} | BaseData
    ].
hrl_enum(EnumKey, EnumValue, BaseData) ->
    EnumKeyUpper = string:to_upper(atom_to_list(EnumKey)),
    [
        {enum_key, EnumKey},
        {enum_key_upper, EnumKeyUpper},
        {enum_value, EnumValue} | BaseData
    ].

add_is_last([]) -> [];
add_is_last(List) ->
    [H | T] = lists:reverse(List),
    lists:reverse([[{is_last, true} | H] | T]).
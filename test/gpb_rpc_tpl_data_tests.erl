-module(gpb_rpc_tpl_data_tests).
-include_lib("eunit/include/eunit.hrl").

gen_router_test_() ->
    meck:new(gpb_rpc_compile),
    MsgBaseString = msg_base_p2_string(),
    {ok, Defines} = gpb_compile:string(mod, MsgBaseString, [to_proto_defs]),
    meck:expect(gpb_rpc_compile, find_proto_file, 4, [{"msg_base", Defines}]),

    RouterFile = "msg",
    ReqList = [
        [
            {cmd, 0},
            {ccmd, 0},
            {msg, "heartbeat_req"},
            {msg_upper, "HEARTBEAT_REQ"},
            {is_empty, true},
            {handle_func, "heartbeat"},
            {mod_cmd, 0},
            {mod_cmd_name, "msg_base"},
            {mod_cmd_name_upper, "MSG_BASE"},
            {gpb_proto, "msg_base_pb"},
            {handle_mod, "mod_base"}
        ],
        [
            {is_last, true},
            {cmd, 2},
            {ccmd, 2},
            {msg, "just_req"},
            {msg_upper, "JUST_REQ"},
            {is_empty, false},
            {handle_func, "just_req"},
            {mod_cmd, 0},
            {mod_cmd_name, "msg_base"},
            {mod_cmd_name_upper, "MSG_BASE"},
            {gpb_proto, "msg_base_pb"},
            {handle_mod, "mod_base"}
        ]
    ],
    RespList = [
        [
            {is_last, true},
            {cmd, 1},
            {ccmd, 1},
            {msg, "heartbeat_resp"},
            {msg_upper, "HEARTBEAT_RESP"},
            {is_empty, true},
            {handle_func, "heartbeat"},
            {mod_cmd, 0},
            {mod_cmd_name, "msg_base"},
            {mod_cmd_name_upper, "MSG_BASE"},
            {gpb_proto, "msg_base_pb"},
            {handle_mod, "mod_base"}
        ]
    ],
    CmdList = [
        [
            {cmd, 0},
            {ccmd, 0},
            {msg, "heartbeat_req"},
            {msg_upper, "HEARTBEAT_REQ"},
            {is_empty, true},
            {handle_func, "heartbeat"},
            {mod_cmd, 0},
            {mod_cmd_name, "msg_base"},
            {mod_cmd_name_upper, "MSG_BASE"},
            {gpb_proto, "msg_base_pb"},
            {handle_mod, "mod_base"}
        ],
        [
            {cmd, 1},
            {ccmd, 1},
            {msg, "heartbeat_resp"},
            {msg_upper, "HEARTBEAT_RESP"},
            {is_empty, true},
            {handle_func, "heartbeat"},
            {mod_cmd, 0},
            {mod_cmd_name, "msg_base"},
            {mod_cmd_name_upper, "MSG_BASE"},
            {gpb_proto, "msg_base_pb"},
            {handle_mod, "mod_base"}
        ],
        [
            {is_last, true},
            {cmd, 2},
            {ccmd, 2},
            {msg, "just_req"},
            {msg_upper, "JUST_REQ"},
            {is_empty, false},
            {handle_func, "just_req"},
            {mod_cmd, 0},
            {mod_cmd_name, "msg_base"},
            {mod_cmd_name_upper, "MSG_BASE"},
            {gpb_proto, "msg_base_pb"},
            {handle_mod, "mod_base"}

        ]
    ],

    ErlRenderData = [
        {file, "msg"},
        {file_upper, "MSG"},
        {req_list, ReqList},
        {resp_list, RespList},
        {router_cmd_list, [[{is_last, true}, {cmd_name, msg_base}, {cmd, 0}]]},
        {cmd_list, CmdList}
    ],
    HrlRenderData = [
        {file, "msg"},
        {file_upper, "MSG"},
        {cmd_list, CmdList},
        {hrl_list, [[{hrl_name, "msg_base_pb"}]]}
    ],

    String = router_string(),
    % {RouterFile, ErlRenderData, HrlRenderData} =
    RouterModData = router_mod_data(String),
    meck:unload(gpb_rpc_compile),

%%    ?debugFmt("~p", [RouterModData]),

    [
        ?_assertEqual(RouterFile, element(1, RouterModData)),
        ?_assertEqual(ErlRenderData, element(2, RouterModData)),
        ?_assertEqual(HrlRenderData, element(3, RouterModData))
    ].

no_service_test() ->
    String = "
enum c_cmd{
    heartbeat_req   = 0;
    heartbeat_resp  = 1;
}

message heartbeat_req{}
message heartbeat_resp{}
",
    ?assertEqual(skip, mod_file_data(String)).

gen_mod_test_() ->
    P2String = msg_base_p2_string(),
    % for server
    ExpectData = [
        {file, "msg_base_pb"},
        {gpb_proto, "msg_base_pb"},
        {callback_list, [
            [
                {gpb_proto, "msg_base_pb"},
                {callback, "heartbeat"},
                {req, "heartbeat_req"}
            ],
            [
                {gpb_proto, "msg_base_pb"},
                {callback, "just_req"},
                {req, "just_req"}
            ]
        ]}
    ],

    % for client
    ExpectData_C = [
        {file, "msg_base_pb"},
        {gpb_proto, "msg_base_pb"},
        {callback_list, [
            [
                {gpb_proto, "msg_base_pb"},
                {callback, "heartbeat"},
                {resp, "heartbeat_resp"}
            ]
        ]}
    ],
    P2RenderData = mod_file_data(P2String, server),
    P2RenderData_C = mod_file_data(P2String, client),

    P3String = msg_base_p3_string(),
    P3RenderData = mod_file_data(P3String, server),

    [
        ?_assertEqual(ExpectData, P2RenderData),
        ?_assertEqual(ExpectData_C, P2RenderData_C),
        ?_assertEqual(ExpectData, P3RenderData)
    ].

gen_hrl_test() ->
    String = msg_base_p2_string(),

    ExpectData = [
        {enums_list, [
            [
                {enum_list, [
                    [
                        {enum_key, 'Apple'},
                        {enum_key_upper, "APPLE"},
                        {enum_value, 0},
                        {enum_name, fruit},
                        {enum_name_upper, "FRUIT"},
                        {file, "msg_base"},
                        {file_upper, "MSG_BASE"},
                        {gpb_proto, "msg_base_pb"}
                    ],
                    [
                        {enum_key, 'Banana'},
                        {enum_key_upper, "BANANA"},
                        {enum_value, 1},
                        {enum_name, fruit},
                        {enum_name_upper, "FRUIT"},
                        {file, "msg_base"},
                        {file_upper, "MSG_BASE"},
                        {gpb_proto, "msg_base_pb"}
                    ],
                    [
                        {enum_key, 'Orange'},
                        {enum_key_upper, "ORANGE"},
                        {enum_value, 3},
                        {enum_name, fruit},
                        {enum_name_upper, "FRUIT"},
                        {file, "msg_base"},
                        {file_upper, "MSG_BASE"},
                        {gpb_proto, "msg_base_pb"}
                    ]
                ]},
                {enum_name, fruit},
                {enum_name_upper, "FRUIT"},
                {file, "msg_base"},
                {file_upper, "MSG_BASE"},
                {gpb_proto, "msg_base_pb"}
            ]
        ]},
        {file, "msg_base"},
        {file_upper, "MSG_BASE"},
        {gpb_proto, "msg_base_pb"}
    ],
    RenderData = hrl_file_data(String),
%%    ?debugFmt("~p", [RenderData]),
    ?assertEqual(ExpectData, RenderData).

router_string() ->
    "
enum cmd{
    msg_base   = 0;
}
    ".
msg_base_p2_string() ->
    "
enum c_cmd{
    heartbeat_req   = 0;
    heartbeat_resp  = 1;
    just_req        = 2;
}

enum fruit{
    Apple  = 0;
    Banana = 1;
    Orange = 3;
}

message undefined{}

service msg_base_service{
    rpc heartbeat(heartbeat_req) returns (heartbeat_resp);
    rpc just_req(just_req) returns (undefined);
    rpc heartbeat_2(undefined) returns (heartbeat_resp);
}

message heartbeat_req{}
message heartbeat_resp{}
message just_req{
    required uint64 id = 1;
}
    ".

msg_base_p3_string() ->
    "
syntax = \"proto3\";
import \"google/protobuf/empty.proto\";

enum c_cmd{
    heartbeat_req   = 0;
    heartbeat_resp  = 1;
    just_req        = 2;
}

service msg_base_service{
    rpc heartbeat(heartbeat_req) returns (heartbeat_resp);
    rpc just_req(just_req) returns (Empty);
    rpc heartbeat_2(Empty) returns (heartbeat_resp);
}

message heartbeat_req{}
message heartbeat_resp{}
message just_req{
    uint64 id = 1;
}
    ".

mod_file_data(String) ->
    mod_file_data(String, server).
mod_file_data(String, GenMode) ->
    {ok, Defines} = gpb_compile:string(mod, String, [to_proto_defs]),
    ProtoName = "msg_base",
    ModuleNameSuffix = "_pb",
    FileName = ProtoName ++ ModuleNameSuffix,
    GpbProto = ProtoName ++ ModuleNameSuffix,
    RenderData = gpb_rpc_tpl_data:mod_file_data(FileName, ProtoName, GpbProto, Defines, GenMode),

%%    ?debugFmt("~p", [RenderData]),
    RenderData.

hrl_file_data(String) ->
    {ok, Defines} = gpb_compile:string(mod, String, [to_proto_defs]),
    ProtoName = "msg_base",
    ModuleNameSuffix = "_pb",
    FileName = ProtoName,
    GpbProto = ProtoName ++ ModuleNameSuffix,
    gpb_rpc_tpl_data:hrl_file_data(FileName, ProtoName, GpbProto, Defines).

router_mod_data(String) ->
    AppDir = ".",
    RouterFile = "msg.proto",
    GpbRpcOpts = [
        {router, "proto/msg.proto"},
        {msg_prefix, "msg_"},
        {mod_prefix, "mod_"},
        {o_erl, "src/msg"},
        {o_hrl, "include/msg"},
        {erl_tpl, "templates/gpb_rpc/gpb_rpc.erl.tpl"},
        {hrl_tpl, "templates/gpb_rpc/gpb_rpc.hrl.tpl"},
        {router_erl_tpl, "templates/gpb_rpc/gpb_rpc_router.erl.tpl"},
        {router_hrl_tpl, "templates/gpb_rpc/gpb_rpc_router.hrl.tpl"},
        {module_name_suffix, ""},
        {cmd_bit, 7},
        {ccmd_bit, 9}
    ],
    GpbOpts = [
        {i, "proto"},
        strings_as_binaries,
        {verify, always},
        {o_erl, "src/proto"},
        {o_hrl, "include/proto"},
        {module_name_suffix, "_pb"}
    ],
    {ok, Defines} = gpb_compile:string(mod, String, [to_proto_defs]),
    gpb_rpc_tpl_data:router_mod_data(AppDir, RouterFile, GpbRpcOpts, GpbOpts, Defines).
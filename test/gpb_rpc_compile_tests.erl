-module(gpb_rpc_compile_tests).
-include_lib("eunit/include/eunit.hrl").

gen_mod_skip_test() ->
    String = "
enum c_cmd{
    heartbeat_req   = 0;
    heartbeat_resp  = 1;
}

message heartbeat_req{}
message heartbeat_resp{}
",
    ?assertEqual(skip, gen_mod(String)).

gen_mod_p2_test() ->
    String = "
enum c_cmd{
    heartbeat_req   = 0;
    heartbeat_resp  = 1;
}

message undefined{}

service msg_base_service{
    rpc heartbeat(heartbeat_req) returns (heartbeat_resp);
    rpc heartbeat_1(heartbeat_req) returns (undefined);
    rpc heartbeat_2(undefined) returns (heartbeat_resp);
}

message heartbeat_req{}
message heartbeat_resp{}
",
    RenderData = gen_mod(String),
    ?assertEqual(RenderData, [
        {proto_name, "msg_base"},
        {proto_name_upper, "MSG_BASE"},
        {gpb_proto, "msg_base_pb"},
        {handle_mod, "mod_base"},
        {callback_list, [[{handle_func, "heartbeat"}], [{handle_func, "heartbeat_1"}]]},
        {rpc_list, [
            [
                {input, "heartbeat_req"},
                {input_upper, "HEARTBEAT_REQ"},
                {output, "heartbeat_resp"},
                {output_upper, "HEARTBEAT_RESP"},
                {handle_func, "heartbeat"},
                {proto_name, "msg_base"},
                {proto_name_upper, "MSG_BASE"},
                {gpb_proto, "msg_base_pb"},
                {handle_mod, "mod_base"}
            ]
        ]},
        {input_list, [
            [
                {input, "heartbeat_req"},
                {input_upper, "HEARTBEAT_REQ"},
                {handle_func, "heartbeat_1"},
                {proto_name, "msg_base"},
                {proto_name_upper, "MSG_BASE"},
                {gpb_proto, "msg_base_pb"},
                {handle_mod, "mod_base"}
            ]
        ]},
        {output_list, [
            [
                {output, "heartbeat_resp"},
                {output_upper, "HEARTBEAT_RESP"},
                {proto_name, "msg_base"},
                {proto_name_upper, "MSG_BASE"},
                {gpb_proto, "msg_base_pb"},
                {handle_mod, "mod_base"}
            ]
        ]},
        {proto_name, "msg_base"},
        {proto_name_upper, "MSG_BASE"},
        {gpb_proto, "msg_base_pb"},
        {handle_mod, "mod_base"}
    ]).


gen_mod_p3_test() ->
    String = "
syntax = \"proto3\";
import \"google/protobuf/empty.proto\";

enum c_cmd{
    heartbeat_req   = 0;
    heartbeat_resp  = 1;
}

service msg_base_service{
    rpc heartbeat(heartbeat_req) returns (heartbeat_resp);
    rpc heartbeat_1(heartbeat_req) returns (Empty);
    rpc heartbeat_2(Empty) returns (heartbeat_resp);
}

message heartbeat_req{}
message heartbeat_resp{}

",
    RenderData = gen_mod(String),
    ?assertEqual(RenderData, [
        {proto_name, "msg_base"},
        {proto_name_upper, "MSG_BASE"},
        {gpb_proto, "msg_base_pb"},
        {handle_mod, "mod_base"},
        {callback_list, [[{handle_func, "heartbeat"}], [{handle_func, "heartbeat_1"}]]},
        {rpc_list, [
            [
                {input, "heartbeat_req"},
                {input_upper, "HEARTBEAT_REQ"},
                {output, "heartbeat_resp"},
                {output_upper, "HEARTBEAT_RESP"},
                {handle_func, "heartbeat"},
                {proto_name, "msg_base"},
                {proto_name_upper, "MSG_BASE"},
                {gpb_proto, "msg_base_pb"},
                {handle_mod, "mod_base"}
            ]
        ]},
        {input_list, [
            [
                {input, "heartbeat_req"},
                {input_upper, "HEARTBEAT_REQ"},
                {handle_func, "heartbeat_1"},
                {proto_name, "msg_base"},
                {proto_name_upper, "MSG_BASE"},
                {gpb_proto, "msg_base_pb"},
                {handle_mod, "mod_base"}
            ]
        ]},
        {output_list, [
            [
                {output, "heartbeat_resp"},
                {output_upper, "HEARTBEAT_RESP"},
                {proto_name, "msg_base"},
                {proto_name_upper, "MSG_BASE"},
                {gpb_proto, "msg_base_pb"},
                {handle_mod, "mod_base"}
            ]
        ]},
        {proto_name, "msg_base"},
        {proto_name_upper, "MSG_BASE"},
        {gpb_proto, "msg_base_pb"},
        {handle_mod, "mod_base"}
    ]).

gen_mod(String) ->
    {ok, Defs} = gpb_compile:string(mod, String, [to_proto_defs]),
    ProtoName = "msg_base",
    ModuleNameSuffix = "_pb",
    PrefixLen = length("msg_"),
    ModPrefix = "mod_",
    RenderData = gpb_rpc_compile:gen_mod(ProtoName, ModuleNameSuffix, PrefixLen, ModPrefix, Defs),

%%    ?debugFmt("~p", [RenderData]),
    RenderData.

gen_hrl_test() ->
    String = "
enum c_cmd{
    heartbeat_req   = 0;
    heartbeat_resp  = 1;
}

message undefined{}

service msg_base_service{
    rpc heartbeat(heartbeat_req) returns (heartbeat_resp);
    rpc heartbeat_1(heartbeat_req) returns (undefined);
    rpc heartbeat_2(undefined) returns (heartbeat_resp);
}

message heartbeat_req{}
message heartbeat_resp{}
",
    {ok, Defs} = gpb_compile:string(mod, String, [to_proto_defs]),
    ProtoName = "msg_base",
    RenderData = gpb_rpc_compile:gen_hrl(ProtoName, Defs),
%%    ?debugFmt("~p", [RenderData]),

    ?assertEqual(RenderData, [
        {enums_list, [
            [
                {enum_list, [
                    [{enum_key, heartbeat_req},
                        {enum_key_upper, "HEARTBEAT_REQ"},
                        {enum_value, 0},
                        {enum_name, c_cmd},
                        {enum_name_upper, "C_CMD"},
                        {proto_name, "msg_base"},
                        {proto_name_upper, "MSG_BASE"}],
                    [{enum_key, heartbeat_resp},
                        {enum_key_upper, "HEARTBEAT_RESP"},
                        {enum_value, 1},
                        {enum_name, c_cmd},
                        {enum_name_upper, "C_CMD"},
                        {proto_name, "msg_base"},
                        {proto_name_upper, "MSG_BASE"}]]},
                {enum_name, c_cmd},
                {enum_name_upper, "C_CMD"},
                {proto_name, "msg_base"},
                {proto_name_upper, "MSG_BASE"}
            ]
        ]},
        {proto_name, "msg_base"},
        {proto_name_upper, "MSG_BASE"}
    ]).

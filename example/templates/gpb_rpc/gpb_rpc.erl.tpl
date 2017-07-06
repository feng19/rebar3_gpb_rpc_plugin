-module({{proto_name}}).

-compile(inline).

-include("msg.hrl").
-ifdef(CMD_{{proto_name_upper}}).
-include("{{proto_name}}.hrl").
-include("{{gpb_proto}}.hrl").

-define(THIS_CMD, ?CMD_{{proto_name_upper}}).

-export([
    handle_msg/3,
    decode_input_msg/1,
    decode_output_msg/1,
    decode_input/2,
    decode_output/2,
    encode_msg/2,
    send_msg/3,
    handle_msg/4
]).

{{#callback_list}}
-callback {{handle_func}}(Msg :: bg_msg:msg(), State :: bg_msg:state()) -> Reply :: bg_msg:func_reply().
{{/callback_list}}

{{#rpc_list}}
handle_msg(?C_CMD_{{input_upper}}, Binary, State) ->
    Msg = {{gpb_proto}}:decode_msg(Binary, {{input}}),
    Maps = {{handle_mod}}:{{handle_func}}(Msg, State),
    case maps:find(msg, Maps) of
        {ok, RespMsg} ->
            PbBinary = {{gpb_proto}}:encode_msg(RespMsg),
            RespBinary = <<?THIS_CMD:7, ?C_CMD_{{output_upper}}:9, PbBinary/binary>>,
            case maps:find(resp_binary, Maps) of
                {ok, RespBinaryList} when is_list(RespBinaryList) ->
                    Maps#{resp_binary => [RespBinary | RespBinaryList]};
                {ok, OldRespBinary} ->
                    Maps#{resp_binary => [RespBinary, OldRespBinary]};
                error ->
                    Maps#{resp_binary => RespBinary}
            end;
        error -> Maps
    end;
{{/rpc_list}}
{{#input_list}}
handle_msg(?C_CMD_{{input_upper}}, Binary, State) ->
    Msg = {{gpb_proto}}:decode_msg(Binary, {{input}}),
    {{handle_mod}}:{{handle_func}}(Msg, State);
{{/input_list}}
handle_msg(CCmd, _Binary, _State) ->
    #{error => {not_defined_c_cmd, CCmd}}.

decode_input_msg(<<?THIS_CMD:7, CCmd:9, MsgBinary/binary>>) ->
    decode_input(CCmd, MsgBinary).

decode_output_msg(<<?THIS_CMD:7, CCmd:9, MsgBinary/binary>>) ->
    decode_output(CCmd, MsgBinary).

{{#rpc_list}}
handle_msg(?C_CMD_{{input_upper}}, MsgType, Binary, State) ->
    Msg = {{gpb_proto}}:decode_msg(Binary, {{input}}),
    {{handle_mod}}:{{handle_func}}({MsgType, Msg}, State);
{{/rpc_list}}
{{#input_list}}
handle_msg(?C_CMD_{{input_upper}}, MsgType, Binary, State) ->
    Msg = {{gpb_proto}}:decode_msg(Binary, {{input}}),
    {{handle_mod}}:{{handle_func}}({MsgType, Msg}, State);
{{/input_list}}
handle_msg(CCmd, _Msg, _Binary, _State) ->
    {error, {not_defined_c_cmd, CCmd}}.

{{#rpc_list}}
decode_input(?C_CMD_{{input_upper}}, MsgBinary) ->
    {{{handle_mod}}, {{handle_func}}, {{gpb_proto}}:decode_msg(MsgBinary, {{input}})};
{{/rpc_list}}
{{#input_list}}
decode_input(?C_CMD_{{input_upper}}, MsgBinary) ->
    {{{handle_mod}}, {{handle_func}}, {{gpb_proto}}:decode_msg(MsgBinary, {{input}})};
{{/input_list}}
decode_input(CCmd, _MsgBinary) ->
    {error, {not_defined_c_cmd, CCmd}}.

{{#output_list}}
decode_output(?C_CMD_{{output_upper}}, MsgBinary) ->
    {{gpb_proto}}:decode_msg(MsgBinary, {{output}});
{{/output_list}}
decode_output(CCmd, _MsgBinary) ->
    {error, {not_defined_c_cmd, CCmd}}.

encode_msg(CCmd, RespMsg) ->
    RespBinary = {{gpb_proto}}:encode_msg(RespMsg),
    <<?THIS_CMD:7, CCmd:9, RespBinary/binary>>.

send_msg(undefined, _CCmd, _Msg) -> undefined;
send_msg(Pid, CCmd, Msg) ->
    Binary = encode_msg(CCmd, Msg),
    Pid ! {send, Binary},
    ok.

-endif.
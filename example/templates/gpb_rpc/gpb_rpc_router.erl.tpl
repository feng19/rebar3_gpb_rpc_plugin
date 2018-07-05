-module({{file}}).
-include("{{file}}.hrl").

%% API
-export([
    handle_msg/3,
    encode_req_msg/1,
    encode_resp_msg/1,
    decode_req_msg/1,
    decode_resp_msg/1
]).

-compile(inline).

-spec handle_msg(ex_msg:cmd16(), ex_msg:request(), ex_msg:state()) -> ex_msg:resp_maps().
{{#input_list}}
handle_msg(?CMD_{{mod_cmd_name_upper}}_{{msg_upper}}, Binary, State) ->
    Msg = {{gpb_proto}}:decode_msg(Binary, {{msg}}),
    Maps = {{handle_mod}}:{{handle_func}}(Msg, State),
    handle_resp_maps(Maps);
{{/input_list}}
handle_msg(Cmd, _Binary, _State) ->
    #{error => {not_defined_cmd, Cmd}}.

-spec handle_resp_maps(map()) -> ex_msg:resp_maps().
handle_resp_maps(Maps) ->
    case maps:find(msg_list, Maps) of
        {ok, MsgList} ->
            BinList = [msg:encode_resp_msg(Msg) || Msg <- MsgList],
            case maps:find(resp_bin_list, Maps) of
                {ok, RespBinList} ->
                    Maps#{resp_bin_list => BinList ++ RespBinList};
                error ->
                    Maps#{resp_bin_list => BinList}
            end;
        error -> Maps
    end.

%% just req
-spec encode_req_msg(ex_msg:msg()) -> binary().
encode_req_msg(Msg) ->
    encode_req_msg(element(1, Msg), Msg).
-spec encode_req_msg(atom(), ex_msg:msg()) -> binary().
{{#input_list}}
encode_req_msg({{msg}}, Msg) ->
    ReqBinary = {{gpb_proto}}:encode_msg(Msg),
    <<?CMD_{{mod_cmd_name_upper}}_{{msg_upper}}:16, ReqBinary/binary>>{{#is_last}}.{{/is_last}}{{^is_last}};{{/is_last}}
{{/input_list}}

%% just resp
-spec encode_resp_msg(ex_msg:msg()) -> binary().
encode_resp_msg(Msg) ->
    encode_resp_msg(element(1, Msg), Msg).
-spec encode_resp_msg(atom(), ex_msg:msg()) -> binary().
{{#output_list}}
encode_resp_msg({{msg}}, Msg) ->
    RespBinary = {{gpb_proto}}:encode_msg(Msg),
    <<?CMD_{{mod_cmd_name_upper}}_{{msg_upper}}:16, RespBinary/binary>>{{#is_last}}.{{/is_last}}{{^is_last}};{{/is_last}}
{{/output_list}}

%% just req
-spec decode_req_msg(binary()) -> ex_msg:msg().
{{#input_list}}
decode_req_msg(<<?CMD_{{mod_cmd_name_upper}}_{{msg_upper}}:16, Binary/binary>>) ->
    {{gpb_proto}}:decode_msg(Binary, {{msg}}){{#is_last}}.{{/is_last}}{{^is_last}};{{/is_last}}
{{/input_list}}

%% just resp
-spec decode_resp_msg(binary()) -> ex_msg:msg().
{{#output_list}}
decode_resp_msg(<<?CMD_{{mod_cmd_name_upper}}_{{msg_upper}}:16, Binary/binary>>) ->
    {{gpb_proto}}:decode_msg(Binary, {{msg}}){{#is_last}}.{{/is_last}}{{^is_last}};{{/is_last}}
{{/output_list}}

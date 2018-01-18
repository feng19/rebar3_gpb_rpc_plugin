Rebar3 gpb rpc plugin
=====

原来 [gpb](https://github.com/tomas-abrahamsson/gpb) 编译出来的文件并没有实现`service`,但是项目需要使用到这个功能做为协议的路由定义,因此有了这个插件

## 应用场景

插件最最要的功能还是通过`gpb`分析协议,然后映射到自定义模板从而生成目标文件

## 例子

[例子](/example) 比较简单,只有一个心跳协议

## 协议解析

[msg.proto](example/proto/msg.proto):

```protobuf
enum cmd{
    msg_base = 0;
}
```

这个文件可以称为`路由模块`,这个文件只定义了一个名为`cmd`的`枚举(enum)`,其意思为`一级协议号(cmd)`,里面的每一个`Key`必须跟`协议的文件名`一致,其值表示`一级协议号(cmd)`

如上: `msg_base` => `msg_base.proto`

[msg_base.proto](example/proto/msg_base.proto):

```protobuf
syntax = "proto3";
import "google/protobuf/empty.proto";

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
```

从`路由模块`索引到这个这个协议文件,这个文件可以称为`二级路由模块`,里面定义了一个名为`c_cmd`的`枚举(enum)`,其意思为`二级协议号(c_cmd)`,里面的每一个`Key`必须跟`消息名(message)`一致,其值表示`二级协议号(c_cmd)`

另外还需要定义一个`service`,`service`名前缀必须与文件名一直并以`_service`结尾

### proto3版本

里面定义的每一个`rpc`表示:

* 同步协议

  ```protobuf
  rpc heartbeat(heartbeat_req) returns (heartbeat_resp);
  ## 格式如: rpc func(req_message) returns (resp_message);
  ## 意思是: 当服务器收到req_message消息时,会调用func方法,然后发送返回resp_message消息给客户端
  ```

* 异步协议

  ```protobuf
  rpc heartbeat_1(heartbeat_req) returns (Empty);
  ## 格式如: rpc func(req_message) returns (Empty);
  ## 意思是: 当服务器收到req_message消息时,会调用func方法,没有消息返回给客户端

  rpc heartbeat_2(Empty) returns (heartbeat_resp);
  ## 格式如: rpc func(Empty) returns (resp_message);
  ## 意思是: 服务器会主动发送resp_message消息给客户端
  ```

### proto2版本

里面定义的每一个`rpc`表示:

>  proto2版本没有`import "google/protobuf/empty.proto";` 因此需要我们自己定义一个 `undefined`的消息,然后引入到每个协议文件

* 同步协议

  ```protobuf
  rpc heartbeat(heartbeat_req) returns (heartbeat_resp);
  ## 格式如: rpc func(req_message) returns (resp_message);
  ## 意思是: 当服务器收到req_message消息时,会调用func方法,然后发送返回resp_message消息给客户端
  ```

* 异步协议

  ```protobuf
  rpc heartbeat_1(heartbeat_req) returns (undefined);
  ## 格式如: rpc func(req_message) returns (undefined);
  ## 意思是: 当服务器收到req_message消息时,会调用func方法,没有消息返回给客户端

  rpc heartbeat_2(undefined) returns (heartbeat_resp);
  ## 格式如: rpc func(undefined) returns (resp_message);
  ## 意思是: 服务器会主动发送resp_message消息给客户端
  ```

### 使用配置

[rebar.config](example/rebar.config)

```erlang
%% for make *.erl file
{erl_opts, [
    warnings_as_errors,
    {i, "_build/default/plugins/gpb/include"}
]}.

%% 增加插件
{plugins, [
    {rebar3_gpb_rpc_plugin, "0.2.0"},
    {rebar3_gpb_plugin, "1.11.6"}
]}.

%% 增加钩子
%% for make *.proto file
{provider_hooks, [
    {pre, [
%%      for gpb
        {compile, {protobuf, compile}},
        {clean, {protobuf, clean}},
%%      for gpb rpc
        {compile, {gpb_rpc, compile}},
        {clean, {gpb_rpc, clean}}
    ]}
]}.

%% gpb的配置
{gpb_opts, [
    {i, "proto"},
    strings_as_binaries,
%%    use_packages
%%    {nif, true},
    {verify, always},
    {o_erl, "src/proto"},
    {o_hrl, "include/proto"},
    {module_name_suffix, "_pb"}
]}.

%% gpb_rpc的配置
{gpb_rpc_opts, [
    {msg_prefix, "msg_"},  %% 设置协议前缀
    {mod_prefix, "mod_"},  %% 设置生成的erl文件名前缀
    {o_erl, "src/msg"},  %% 输出的erl文件目录
    {o_hrl, "include/msg"}, %% 输出的hrl文件目录
    {erl_tpl, "templates/gpb_rpc/gpb_rpc.erl.tpl"}, %% erl模板地址
    {hrl_tpl, "templates/gpb_rpc/gpb_rpc.hrl.tpl"}  %% hrl模板地址
]}.
```

## 编译结果

```shell
$ make co
```

进入example目录执行上面命令,之后看一下效果

先看[erl模板文件](example/templates/gpb_rpc/gpb_rpc.erl.tpl):

```erlang
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
-callback {{handle_func}}(ex_msg:msg(), ex_msg:state()) -> ex_msg:func_reply().
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
```

再来看看生成的文件`src/msg/msg_base.erl`:

```erlang
%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_rpc_compile
-module(msg_base).

-compile(inline).

-include("msg.hrl").
-ifdef(CMD_MSG_BASE).
-include("msg_base.hrl").
-include("msg_base_pb.hrl").

-define(THIS_CMD, ?CMD_MSG_BASE).

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

-callback heartbeat(ex_msg:msg(), ex_msg:state()) -> ex_msg:func_reply().
-callback heartbeat_1(ex_msg:msg(), ex_msg:state()) -> ex_msg:func_reply().

handle_msg(?C_CMD_HEARTBEAT_REQ, Binary, State) ->
    Msg = msg_base_pb:decode_msg(Binary, heartbeat_req),
    Maps = mod_base:heartbeat(Msg, State),
    case maps:find(msg, Maps) of
        {ok, RespMsg} ->
            PbBinary = msg_base_pb:encode_msg(RespMsg),
            RespBinary = <<?THIS_CMD:7, ?C_CMD_HEARTBEAT_RESP:9, PbBinary/binary>>,
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
handle_msg(?C_CMD_HEARTBEAT_REQ, Binary, State) ->
    Msg = msg_base_pb:decode_msg(Binary, heartbeat_req),
    mod_base:heartbeat_1(Msg, State);
handle_msg(CCmd, _Binary, _State) ->
    #{error => {not_defined_c_cmd, CCmd}}.

decode_input_msg(<<?THIS_CMD:7, CCmd:9, MsgBinary/binary>>) ->
    decode_input(CCmd, MsgBinary).

decode_output_msg(<<?THIS_CMD:7, CCmd:9, MsgBinary/binary>>) ->
    decode_output(CCmd, MsgBinary).

handle_msg(?C_CMD_HEARTBEAT_REQ, MsgType, Binary, State) ->
    Msg = msg_base_pb:decode_msg(Binary, heartbeat_req),
    mod_base:heartbeat({MsgType, Msg}, State);
handle_msg(?C_CMD_HEARTBEAT_REQ, MsgType, Binary, State) ->
    Msg = msg_base_pb:decode_msg(Binary, heartbeat_req),
    mod_base:heartbeat_1({MsgType, Msg}, State);
handle_msg(CCmd, _Msg, _Binary, _State) ->
    {error, {not_defined_c_cmd, CCmd}}.

decode_input(?C_CMD_HEARTBEAT_REQ, MsgBinary) ->
    {mod_base, heartbeat, msg_base_pb:decode_msg(MsgBinary, heartbeat_req)};
decode_input(?C_CMD_HEARTBEAT_REQ, MsgBinary) ->
    {mod_base, heartbeat_1, msg_base_pb:decode_msg(MsgBinary, heartbeat_req)};
decode_input(CCmd, _MsgBinary) ->
    {error, {not_defined_c_cmd, CCmd}}.

decode_output(?C_CMD_HEARTBEAT_RESP, MsgBinary) ->
    msg_base_pb:decode_msg(MsgBinary, heartbeat_resp);
decode_output(CCmd, _MsgBinary) ->
    {error, {not_defined_c_cmd, CCmd}}.

encode_msg(CCmd, RespMsg) ->
    RespBinary = msg_base_pb:encode_msg(RespMsg),
    <<?THIS_CMD:7, CCmd:9, RespBinary/binary>>.

send_msg(undefined, _CCmd, _Msg) -> undefined;
send_msg(Pid, CCmd, Msg) ->
    Binary = encode_msg(CCmd, Msg),
    Pid ! {send, Binary},
    ok.
```

-----

再看看[hrl模板文件](example/templates/gpb_rpc/gpb_rpc.hrl.tpl):

```erl
-ifndef({{proto_name_upper}}_H).
-define({{proto_name_upper}}_H, true).

{{#enums_list}}
%% {{enum_name}}
{{#enum_list}}
-define({{enum_name_upper}}_{{enum_key_upper}}, {{enum_value}}). %% {{enum_key}}
-define({{enum_name_upper}}_{{enum_key_upper}}_KEY, '{{enum_key}}'). %% {{enum_value}}
{{/enum_list}}

{{/enums_list}}
-endif.
```

看看生成的文件:

`msg.hrl`:

```erlang
%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_rpc_compile
-ifndef(MSG_H).
-define(MSG_H, true).

%% cmd
-define(CMD_MSG_BASE, 0). %% msg_base
-define(CMD_MSG_BASE_KEY, 'msg_base'). %% 0
```

`msg_base.hrl`:

```erlang
%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_rpc_compile
-ifndef(MSG_BASE_H).
-define(MSG_BASE_H, true).

%% c_cmd
-define(C_CMD_HEARTBEAT_REQ, 0). %% heartbeat_req
-define(C_CMD_HEARTBEAT_REQ_KEY, 'heartbeat_req'). %% 0
-define(C_CMD_HEARTBEAT_RESP, 1). %% heartbeat_resp
-define(C_CMD_HEARTBEAT_RESP_KEY, 'heartbeat_resp'). %% 1
```

## 使用

`example.erl`:

```erlang
-module(example).

%% API exports
-export([
    handle_msg/2,
    mk_resp/0
]).

-include("msg_base.hrl").
-include("msg_base_pb.hrl").

%%====================================================================
%% API functions
%%====================================================================

handle_msg(<<Cmd:7, CCmd:9, Binary/binary>>, State) ->
    MsgMod = msg_pb:enum_symbol_by_value_cmd(Cmd),
    MsgMod:handle_msg(CCmd, Binary, State).

mk_resp() ->
    msg_base:encode_msg(?C_CMD_HEARTBEAT_RESP, #heartbeat_resp{}).
```

当外部需要处理消息是,只需要调用`example:handle_msg(Binary, State)`之后会根据之前定义好的协议头和方法去分发消息处理,以上面心跳的例子来说,当收到`<<0:7, 0:9, Binary/binary>>`的协议时,`Cmd=0`, `CCmd=0`,转换成枚举就是`Cmd=msg_base`,`CCmd=heartbeat_req`,然后会分发到处理函数`mod_base:heartbeat(#heartbeat_req{}, State)`

`mod_base.erl`:

```erlang
-module(mod_base).
-behavior(msg_base).
%% API
-export([
    heartbeat/2,
    heartbeat_1/2
]).

-include("msg_base.hrl").
-include("msg_base_pb.hrl").

heartbeat(#heartbeat_req{}, _State) ->
    #{msg => #heartbeat_resp{}}.

heartbeat_1(#heartbeat_req{}, _State) ->
    #{}.
```


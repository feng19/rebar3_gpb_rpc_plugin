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

%%====================================================================
%% Internal functions
%%====================================================================

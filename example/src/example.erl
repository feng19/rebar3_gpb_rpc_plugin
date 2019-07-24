-module(example).

%% API exports
-export([
    handle_msg/2,
    mk_resp/0
]).

-include("msg_base_pb.hrl").

%%====================================================================
%% API functions
%%====================================================================

handle_msg(<<Cmd:16, Binary/binary>>, State) ->
    msg:handle_msg(Cmd, Binary, State).

mk_resp() ->
    msg:encode_resp_msg(#heartbeat_resp{}).

%%====================================================================
%% Internal functions
%%====================================================================

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
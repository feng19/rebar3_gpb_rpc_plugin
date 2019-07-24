-module(mod_base).
-behavior(msg_base).
%% API
-export([
    heartbeat/2,
    just_req/2
]).

-include("msg_base_pb.hrl").

heartbeat(#heartbeat_req{}, _State) ->
    #{msg => #heartbeat_resp{}}.

just_req(_JustReq, _State) ->
    #{}.
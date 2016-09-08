-module(rebar3_gpb_rpc_compiler).

-export([compile/1,
         clean/1]).

-define(DEFAULT_PROTO_DIR, "proto").
-define(DEFAULT_OUT_ERL_DIR, "src/rpc").
-define(DEFAULT_OUT_HRL_DIR, "include/rpc").
-define(DEFAULT_MODULE_SUFFIX, "").
-define(DEFAULT_HEADER_MSG, "msg").
-define(DEFAULT_MSG_PREFIX, "msg_").

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(rebar_app_info:t()) -> ok.
compile(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    AppOutDir = rebar_app_info:out_dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    {ok, GpbOpts} = dict:find(gpb_opts, Opts),
    {ok, GpbRpcOpts0} = dict:find(gpb_rpc_opts, Opts),
    %% check if non-recursive
    Recursive = proplists:get_value(recursive, GpbRpcOpts0, true),
    HeaderMsg = proplists:get_value(h_msg, GpbRpcOpts0, ?DEFAULT_HEADER_MSG),
    MsgPrefix = proplists:get_value(msg_prefix, GpbRpcOpts0, ?DEFAULT_MSG_PREFIX),
    ModuleNameSuffix = proplists:get_value(module_name_suffix, GpbOpts,
                                           ?DEFAULT_MODULE_SUFFIX),
    SourceDirs = proplists:get_all_values(i, GpbOpts),
    TargetErlDir = filename:join([AppOutDir,
                                  proplists:get_value(o_erl, GpbRpcOpts0,
                                                      ?DEFAULT_OUT_ERL_DIR)]),
    TargetHrlDir = filename:join([AppOutDir,
                                  proplists:get_value(o_hrl, GpbRpcOpts0,
                                                      ?DEFAULT_OUT_HRL_DIR)]),
    rebar_api:debug("making sure that target erl dir ~p exists", [TargetErlDir]),
    ok = ensure_dir(TargetErlDir),
    rebar_api:debug("making sure that target hrl dir ~p exists", [TargetHrlDir]),
    ok = ensure_dir(TargetHrlDir),
    rebar_api:debug("reading proto files from ~p, generating \".erl\" to ~p "
                    "and \".hrl\" to ~p",
      [SourceDirs, TargetErlDir, TargetHrlDir]),
    %% set the full path for the output directories
    %% remove the plugin specific options since gpb will not understand them
    GpbRpcOpts = module_name_suffix_opt(ModuleNameSuffix,
                msg_prefix_opt(MsgPrefix,
                    header_msg_opt(HeaderMsg,
                        target_erl_opt(TargetErlDir,
                            target_hrl_opt(TargetHrlDir, GpbRpcOpts0))))),
    lists:foreach(fun(SourceDir) ->
                    ok = rebar_base_compiler:run(Opts, [],
                                 filename:join(AppDir, SourceDir), ".proto",
                                 TargetErlDir, ".erl",
                                 fun(Source, Target, Config) ->
                                    compile(Source, Target, GpbRpcOpts, Config)
                                 end,
                                 [check_last_mod, {recursive, Recursive}])
                  end, SourceDirs),

    update_erl_first_files(TargetErlDir, AppInfo).

-spec clean(rebar_app_info:t()) -> ok.
clean(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    AppOutDir = rebar_app_info:out_dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    {ok, GpbOpts} = dict:find(gpb_opts, Opts),
    {ok, GpbRpcOpts} = dict:find(gpb_rpc_opts, Opts),
    TargetErlDir = filename:join([AppOutDir,
                                  proplists:get_value(o_erl, GpbRpcOpts,
                                                      ?DEFAULT_OUT_ERL_DIR)]),
    TargetHrlDir = filename:join([AppOutDir,
                                  proplists:get_value(o_hrl, GpbRpcOpts,
                                                      ?DEFAULT_OUT_HRL_DIR)]),
    ProtoFiles = find_proto_files(AppDir, GpbOpts),
    GeneratedRootFiles = [filename:rootname(filename:basename(ProtoFile)) ||
                            ProtoFile <- ProtoFiles],
    GeneratedErlFiles = [filename:join([TargetErlDir, F ++ ".erl"]) ||
                            F <- GeneratedRootFiles],
    GeneratedHrlFiles = [filename:join([TargetHrlDir, F ++ ".hrl"]) ||
                            F <- GeneratedRootFiles],
    rebar_api:debug("deleting [~p, ~p]",
      [GeneratedErlFiles, GeneratedHrlFiles]),
    rebar_file_utils:delete_each(GeneratedErlFiles ++ GeneratedHrlFiles).

%% ===================================================================
%% Private API
%% ===================================================================
-spec compile(string(), string(), proplists:proplist(), term()) -> ok.
compile(Source, _Target, GpbRpcOpts, _Config) ->
    rebar_api:debug("compiling ~p", [Source]),
    rebar_api:debug("opts: ~p", [GpbRpcOpts]),
    case gpb_rpc_compile:file(Source, GpbRpcOpts) of
        ok ->
            ok;
        {error, Reason} ->
            ReasonStr = gpb_compile:format_error(Reason),
            rebar_utils:abort("failed to compile ~s: ~s~n", [Source, ReasonStr])
    end.

-spec ensure_dir(filelib:dirname()) -> 'ok' | {error, Reason::file:posix()}.
ensure_dir(OutDir) ->
  %% Make sure that ebin/ exists and is on the path
  case filelib:ensure_dir(filename:join(OutDir, "dummy.beam")) of
    ok -> ok;
    {error, eexist} ->
      rebar_utils:abort("unable to ensure dir ~p, is it maybe a broken symlink?",
        [OutDir]);
    {error, Reason} -> {error, Reason}
  end.

-spec target_erl_opt(string(), proplists:proplist()) -> proplists:proplist().
target_erl_opt(Dir, Opts) ->
    lists:keystore(o_erl, 1, Opts, {o_erl, Dir}).

-spec target_hrl_opt(string(), proplists:proplist()) -> proplists:proplist().
target_hrl_opt(Dir, Opts) ->
    lists:keystore(o_hrl, 1, Opts, {o_hrl, Dir}).

-spec header_msg_opt(string(), proplists:proplist()) -> proplists:proplist().
header_msg_opt(HeaderMsg, Opts) ->
    lists:keystore(h_msg, 1, Opts, {h_msg, HeaderMsg}).

-spec msg_prefix_opt(string(), proplists:proplist()) -> proplists:proplist().
msg_prefix_opt(MsgPrefix, Opts) ->
    lists:keystore(msg_prefix, 1, Opts, {msg_prefix, MsgPrefix}).

-spec module_name_suffix_opt(string(), proplists:proplists()) -> proplists:proplist().
module_name_suffix_opt(ModuleNameSuffix, Opts) ->
    lists:keystore(module_name_suffix, 1, Opts, {module_name_suffix, ModuleNameSuffix}).

find_proto_files(AppDir, GpbOpts) ->
    lists:foldl(fun(SourceDir, Acc) ->
                Acc ++ rebar_utils:find_files(filename:join(AppDir, SourceDir),
                                   ".*\.proto\$")
              end, [], proplists:get_all_values(i, GpbOpts)).

update_erl_first_files(TargetErlDir, AppInfo) ->
    case filelib:wildcard(filename:join(TargetErlDir, "*.erl")) of
        [] -> AppInfo;
        ErlFirstFiles ->
            OldOpts = rebar_app_info:opts(AppInfo),
            NewOpts =
                case dict:find(erl_first_files, OldOpts) of
                    {ok, OldErlFirstFiles} ->
                        dict:store(erl_first_files, OldErlFirstFiles++ErlFirstFiles, OldOpts);
                    error ->
                        dict:store(erl_first_files, ErlFirstFiles, OldOpts)
                end,
            rebar_app_info:opts(AppInfo, NewOpts)
    end.

-module(rebar3_gpb_rpc_compiler).

-export([
    compile/1,
    clean/1
]).

-define(OPT_KEYS, [
    recursive, msg_prefix, mod_prefix,
    o_erl, o_hrl, erl_tpl, hrl_tpl,
    router_erl_tpl, router_hrl_tpl,
    router, cmd_bit, ccmd_bit,
    module_name_suffix
]).
-define(DEFAULT_PROTO_DIR, "proto").
-define(DEFAULT_OUT_ERL_DIR, "src/rpc").
-define(DEFAULT_OUT_HRL_DIR, "include/rpc").
-define(DEFAULT_MODULE_SUFFIX, "").
-define(DEFAULT_MSG_PREFIX, "msg_").
-define(DEFAULT_MOD_PREFIX, "mod_").
-define(DEFAULT_ERL_TPL, "templates/gen_rpc.hrl.tpl").
-define(DEFAULT_HRL_TPL, "templates/gen_rpc.erl.tpl").
-define(DEFAULT_ROUTER_ERL_TPL, "templates/gen_rpc_router.erl.tpl").
-define(DEFAULT_ROUTER_HRL_TPL, "templates/gen_rpc_router.hrl.tpl").
-define(DEFAULT_ROUTER, "msg.proto").
-define(DEFAULT_CMD_BIT, 7).
-define(DEFAULT_CCMD_BIT, 9).

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(rebar_app_info:t()) -> ok.
compile(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    {ok, GpbOpts} = dict:find(gpb_opts, Opts),

    {ok, GpbRpcOpts0} = dict:find(gpb_rpc_opts, Opts),
    SourceDirs = proplists:get_all_values(i, GpbOpts),
    SourceDirsOpts = [{i, SourceDir} || SourceDir <- SourceDirs],
    GpbRpcOpts = handle_opts(?OPT_KEYS, SourceDirsOpts ++ GpbRpcOpts0),

    TargetErlDir0 = proplists:get_value(o_erl, GpbRpcOpts),
    TargetErlDir = filename:join([AppDir, TargetErlDir0]),
    TargetHrlDir0 = proplists:get_value(o_hrl, GpbRpcOpts),
    TargetHrlDir = filename:join([AppDir, TargetHrlDir0]),
    ErlTplFile0 = proplists:get_value(erl_tpl, GpbRpcOpts),
    ErlTpl = bbmustache:parse_file(filename:join([AppDir, ErlTplFile0])),
    HrlTplFile0 = proplists:get_value(hrl_tpl, GpbRpcOpts),
    HrlTpl = bbmustache:parse_file(filename:join([AppDir, HrlTplFile0])),
    RouterFile0 = proplists:get_value(router, GpbRpcOpts),
    RouterFile = filename:join(AppDir, RouterFile0),

    rebar_api:debug("making sure that target erl dir ~p exists", [TargetErlDir]),
    ok = ensure_dir(TargetErlDir),
    rebar_api:debug("making sure that target hrl dir ~p exists", [TargetHrlDir]),
    ok = ensure_dir(TargetHrlDir),
    rebar_api:debug("reading proto files from ~p, generating \".erl\" to ~p "
    "and \".hrl\" to ~p", [SourceDirs, TargetErlDir, TargetHrlDir]),
    rebar_api:debug("opts: ~p", [GpbRpcOpts]),

    NewGpbOpts = remove_plugin_opts(default_include_opts(AppDir, GpbOpts)),

    %% check if non-recursive
    Recursive = proplists:get_value(recursive, GpbRpcOpts),
    [begin
         ok = rebar_base_compiler:run(Opts, [],
             filename:join(AppDir, SourceDir), ".proto", TargetHrlDir, ".hrl",
             fun(Source, Target, Config) ->
                 case filename:basename(Source) == RouterFile of
                     true -> % skipped
                         ok;
                     _ ->
                         compile(Source, Target, ErlTpl, HrlTpl, GpbRpcOpts, NewGpbOpts, Config)
                 end
             end,
             [check_last_mod, {recursive, Recursive}])
     end || SourceDir <- SourceDirs],

    compile_router(RouterFile, AppDir, TargetErlDir, GpbRpcOpts, NewGpbOpts),

    AppInfo1 = update_include_files(TargetHrlDir0, AppInfo),
    AppInfo2 = update_include_files(proplists:get_value(o_hrl, GpbOpts, "include"), AppInfo1),
    update_erl_first_files(TargetErlDir, AppDir, AppInfo2).

-spec clean(rebar_app_info:t()) -> ok.
clean(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    {ok, GpbOpts} = dict:find(gpb_opts, Opts),
    {ok, GpbRpcOpts} = dict:find(gpb_rpc_opts, Opts),
    TargetErlDir = filename:join([AppDir,
        proplists:get_value(o_erl, GpbRpcOpts,
            ?DEFAULT_OUT_ERL_DIR)]),
    TargetHrlDir = filename:join([AppDir,
        proplists:get_value(o_hrl, GpbRpcOpts,
            ?DEFAULT_OUT_HRL_DIR)]),
    ProtoFiles = find_proto_files(AppDir, GpbOpts),
    RouterFile = filename:rootname(filename:basename(proplists:get_value(router, GpbRpcOpts))),
    GeneratedRootFiles = [filename:rootname(filename:basename(ProtoFile)) ||
        ProtoFile <- lists:sort([RouterFile | ProtoFiles])],

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
-spec compile(string(), string(), string(), string(), proplists:proplist(), proplists:proplist(), term()) -> ok.
compile(Source, _Target, ErlTpl, HrlTpl, GpbRpcOpts, GpbOpts, _Config) ->
    rebar_api:debug("compiling ~p", [Source]),
    case gpb_rpc_compile:file(Source, ErlTpl, HrlTpl, GpbRpcOpts, GpbOpts) of
        ok ->
            ok;
        {error, Reason} ->
            ReasonStr = gpb_compile:format_error(Reason),
            rebar_utils:abort("failed to compile ~s: ~s~n", [Source, ReasonStr])
    end.

compile_router(RouterFile, AppDir, TargetErlDir, GpbRpcOpts, GpbOpts) ->
    filelib:is_file(RouterFile) orelse rebar_utils:abort("miss router ~s~n", [RouterFile]),
    RouterExt = filename:extension(RouterFile),
    RpcModuleNameSuffix = proplists:get_value(module_name_suffix, GpbRpcOpts),
    RouterErl0 = filename:basename(RouterFile, RouterExt) ++ RpcModuleNameSuffix ++ ".erl",
    TargetErls0 = filelib:wildcard("*.erl", TargetErlDir),
    case lists:member(RouterErl0, TargetErls0) of
        true ->
            RouterErl = filename:join(TargetErlDir, RouterErl0),
            TargetErls1 = lists:delete(RouterErl0, TargetErls0),
            TargetErls = [filename:join(TargetErlDir, TargetErl) || TargetErl <- TargetErls1],
            [Max | _] = lists:reverse(lists:sort([filelib:last_modified(TargetErl)
                || TargetErl <- [RouterFile | TargetErls]])),
            case filelib:last_modified(RouterErl) < Max of
                true ->
                    gpb_rpc_compile:gen_router(AppDir, RouterFile, GpbRpcOpts, GpbOpts);
                false ->
                    skipped
            end;
        false ->
            gpb_rpc_compile:gen_router(AppDir, RouterFile, GpbRpcOpts, GpbOpts)
    end.

-spec ensure_dir(filelib:dirname()) -> 'ok' | {error, Reason :: file:posix()}.
ensure_dir(OutDir) ->
    %% Make sure that ebin/ exists and is on the path
    case filelib:ensure_dir(filename:join(OutDir, "dummy.beam")) of
        ok -> ok;
        {error, eexist} ->
            rebar_utils:abort("unable to ensure dir ~p, is it maybe a broken symlink?",
                [OutDir]);
        {error, Reason} -> {error, Reason}
    end.

handle_opts([recursive | OptKeys], Opts) ->
    handle_opts_do(recursive, true, OptKeys, Opts);
handle_opts([msg_prefix | OptKeys], Opts) ->
    handle_opts_do(msg_prefix, ?DEFAULT_MSG_PREFIX, OptKeys, Opts);
handle_opts([mod_prefix | OptKeys], Opts) ->
    handle_opts_do(mod_prefix, ?DEFAULT_MOD_PREFIX, OptKeys, Opts);
handle_opts([module_name_suffix | OptKeys], Opts) ->
    handle_opts_do(module_name_suffix, ?DEFAULT_MODULE_SUFFIX, OptKeys, Opts);
handle_opts([o_erl | OptKeys], Opts) ->
    handle_opts_do(o_erl, ?DEFAULT_OUT_ERL_DIR, OptKeys, Opts);
handle_opts([o_hrl | OptKeys], Opts) ->
    handle_opts_do(o_hrl, ?DEFAULT_OUT_HRL_DIR, OptKeys, Opts);
handle_opts([erl_tpl | OptKeys], Opts) ->
    handle_opts_do(erl_tpl, ?DEFAULT_ERL_TPL, OptKeys, Opts);
handle_opts([hrl_tpl | OptKeys], Opts) ->
    handle_opts_do(hrl_tpl, ?DEFAULT_HRL_TPL, OptKeys, Opts);
handle_opts([router_erl_tpl | OptKeys], Opts) ->
    handle_opts_do(router_erl_tpl, ?DEFAULT_ROUTER_ERL_TPL, OptKeys, Opts);
handle_opts([router_hrl_tpl | OptKeys], Opts) ->
    handle_opts_do(router_Hrl_tpl, ?DEFAULT_ROUTER_HRL_TPL, OptKeys, Opts);
handle_opts([router | OptKeys], Opts) ->
    handle_opts_do(router, ?DEFAULT_ROUTER, OptKeys, Opts);
handle_opts([cmd_bit | OptKeys], Opts) ->
    handle_opts_do(cmd_bit, ?DEFAULT_CMD_BIT, OptKeys, Opts);
handle_opts([ccmd_bit | OptKeys], Opts) ->
    handle_opts_do(ccmd_bit, ?DEFAULT_CCMD_BIT, OptKeys, Opts);
handle_opts([], Opts) -> Opts.

handle_opts_do(Key, DefaultValue, OptKeys, Opts) ->
    case lists:keytake(Key, 1, Opts) of
        {value, Value, NewOpts} ->
            handle_opts(OptKeys, lists:keystore(Key, 1, NewOpts, Value));
        false ->
            handle_opts(OptKeys, lists:keystore(Key, 1, Opts, {Key, DefaultValue}))
    end.

find_proto_files(AppDir, GpbOpts) ->
    lists:foldl(fun(SourceDir, Acc) ->
        Acc ++ rebar_utils:find_files(filename:join(AppDir, SourceDir),
            ".*\.proto\$")
                end, [], proplists:get_all_values(i, GpbOpts)).

update_include_files(TargetHrlDir, AppInfo) ->
    OldOpts = rebar_app_info:opts(AppInfo),
    NewOpts =
        case dict:find(erl_opts, OldOpts) of
            {ok, OldErlOpts} ->
                NewErlOpts = lists:usort([{i, TargetHrlDir} | OldErlOpts]),
                dict:store(erl_opts, NewErlOpts, OldOpts);
            error ->
                dict:store(erl_opts, [{i, TargetHrlDir}], OldOpts)
        end,
    rebar_app_info:opts(AppInfo, NewOpts).

update_erl_first_files(TargetErlDir, AppDir, AppInfo) ->
    case filelib:wildcard(filename:join(TargetErlDir, "*.erl")) of
        [] -> AppInfo;
        ErlFirstFiles0 ->
            PrefixLen = length(AppDir),
            ErlFirstFiles = [lists:nthtail(PrefixLen, ErlFirstFile) || ErlFirstFile <- ErlFirstFiles0],
            OldOpts = rebar_app_info:opts(AppInfo),
            NewOpts =
                case dict:find(erl_first_files, OldOpts) of
                    {ok, OldErlFirstFiles} ->
                        dict:store(erl_first_files, OldErlFirstFiles ++ ErlFirstFiles, OldOpts);
                    error ->
                        dict:store(erl_first_files, ErlFirstFiles, OldOpts)
                end,
            rebar_app_info:opts(AppInfo, NewOpts)
    end.

-spec default_include_opts(string(), proplists:proplist()) -> proplists:proplist().
default_include_opts(AppDir, Opts) ->
    lists:map(
        fun({i, Path}) ->
            {i, filename:join(AppDir, Path)};
            ({ipath, Path}) ->
                {i, filename:join(AppDir, Path)};
            (Opt) -> Opt
        end, Opts).

remove_plugin_opts(Opts) ->
    lists:foldl(
        fun(Key, Acc) ->
            lists:keydelete(Key, 1, Acc)
        end, Opts, [recursive, ipath]).

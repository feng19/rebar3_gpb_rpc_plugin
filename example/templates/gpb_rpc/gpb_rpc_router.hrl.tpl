-ifndef({{file_upper}}_H).
-define({{file_upper}}_H, true).

%%%===================================================================
%%% cmd
%%%===================================================================
{{#cmd_list}}
-define(CMD_{{mod_cmd_name_upper}}_{{msg_upper}}, {{cmd}}). % mod_cmd:{{mod_cmd}} ccmd:{{ccmd}}
{{/cmd_list}}

-endif.
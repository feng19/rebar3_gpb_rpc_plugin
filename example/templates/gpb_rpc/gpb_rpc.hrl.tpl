-ifndef({{file_upper}}_H).
-define({{file_upper}}_H, true).

{{#enums_list}}
%% {{enum_name}}
{{#enum_list}}
-define({{enum_name_upper}}_{{enum_key_upper}}, {{enum_value}}). %% {{enum_key}}
-define({{enum_name_upper}}_{{enum_key_upper}}_KEY, '{{enum_key}}'). %% {{enum_value}}
{{/enum_list}}

{{/enums_list}}
-endif.

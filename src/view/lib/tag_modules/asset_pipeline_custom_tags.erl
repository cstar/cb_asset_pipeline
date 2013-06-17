-module(asset_pipeline_custom_tags).
-compile(export_all).

javascript(Variables, _Options) ->
  Src = proplists:get_value(src, Variables),
  Pid = boss_assets_sup:asset_proc( binary_to_list(Src)),
  {ok, Paths} = boss_asset_file:urls(Pid, boss_env:is_developing_app(asset_pipeline)),
  [[<<"<script type=\"text/javascript\" src=\"assets/">>, Path, <<"\"></script>">>] || Path <- Paths ].

%stylesheet(Variables, Options)->
%  Path = binary_to_list(proplists:get_value(src, Variables),
%  boss_files:root_priv
% put custom tags in here, e.g.
%
% reverse(Variables, Options) ->
%     lists:reverse(binary_to_list(proplists:get_value(string, Variables))).
%
% {% reverse string="hello" %} => "olleh"
%
% Variables are the passed-in vars in your template

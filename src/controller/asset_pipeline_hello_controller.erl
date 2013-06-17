-module (asset_pipeline_hello_controller, [Req]).
-compile(export_all).

world('GET', [])->ok.

assets('GET', [File] )->
  string:tokens(File, "-"),
  error_logger:error_msg("Toto ~p", [File]),
  Pid = boss_assets_sup:asset_proc(File),
  {ok, Content, Headers} = boss_asset_file:contents(Pid),
  {output, Content, Headers}.
-module (asset_pipeline_assets_controller, [Req]).
-compile(export_all).

world('GET', [])->ok.

serve('GET', [File] )->
  [Ext, Hash | Rest] = lists:reverse(string:tokens(File, ".")),
  Filename = lists:flatten([lists:reverse(Rest), $., Ext]),
  Headers = set_headers(Hash),
  Pid = boss_assets_sup:asset_proc(Filename),
  {ok, Content, AssetHeaders} = boss_asset_file:compressed(Pid),
  {output, Content, Headers ++ AssetHeaders}.


set_headers("raw") ->
  [];

set_headers(_Hash)->
  [{'cache-control', "public"}, {'expires', simple_bridge_util:expires(years, 10)}].
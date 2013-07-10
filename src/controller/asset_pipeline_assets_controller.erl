-module (asset_pipeline_assets_controller, [Req]).
-compile(export_all).

world('GET', [])->ok.

serve('GET', [File] )->
  [Ext, Hash | Rest] = lists:reverse(string:tokens(File, ".")),
  Filename = lists:flatten([lists:reverse(Rest), $., Ext]),
  Headers = set_headers(),
  {ok, Content, AssetHeaders} = boss_assets:serve(Filename),
  {output, Content, set_headers() ++ AssetHeaders}.


set_headers()->
  [{'cache-control', "public"}, {'expires', simple_bridge_util:expires(years, 10)}].
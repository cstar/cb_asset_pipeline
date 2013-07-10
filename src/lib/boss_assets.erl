-module (boss_assets).

-export ([start/0, urls/1, serve/1, list/0, clean/0]).

start()->
  ets:new(boss_assets, [set, named_table, public]),
  ok.


urls(Asset)->
  case should(concatenate) of
    true ->
      {Name, Hash, Deps, _} = fetch(Asset),
      [get_served_filename(Name, Hash)];
    false ->
      {Name, Hash, Deps, _} = fetch(Asset),
      lists:map(fun urls/1, Deps) ++ [get_served_filename(Name, Hash)]
  end.

serve(Asset) ->
  {Name, Hash, Deps, Content} = fetch(Asset),
  {ok, Content, []}.

fetch(Asset)->
  case ets:lookup(boss_assets, Asset) of
    [] ->
      load_asset(Asset);
    [AssetData|_Rest] ->
      AssetData
  end.

list()->
  lists:usort(ets:foldl(
    fun({Asset, _, Deps, _}, Acc)->
      [fs_path(Asset) | Acc] ++ [fs_path(A) || A <- Deps]
    end, [], boss_assets)).

clean()->
  ets:delete_all_objects(boss_assets). 


load_asset(Asset) ->
  Type = file_to_type(Asset),
  FilePath = fs_path(Asset),
  case file:read_file(FilePath) of
    {ok, Content} ->
      save_asset(Asset, Content, Type, should(concatenate));
    {error, Error} ->
      error_logger:error_msg("~s not found", FilePath)
  end.

fs_path(Asset) ->
  App = boss_env:get_env(asset_pipeline, assets_for, asset_pipeline),
  Type = file_to_type(Asset),
  FilePath= filename:join([boss_files:root_priv_dir(App), "assets", atom_to_list(Type), Asset]).

save_asset(Asset, Content, Type, false)->
  Deps = get_deps(Type, Content),
  lists:map(fun load_asset/1, Deps),
  MaybeMin = maybe_minify(Type, Content),
  Hash = hash(MaybeMin),
  ets:insert_new(boss_assets, {Asset, Hash, Deps, MaybeMin}),
  {Asset, Hash, Deps, MaybeMin};

save_asset(Asset, Content, Type, true)->
  Deps = get_deps(Type, Content),
  IOListContent = lists:foldl(fun(Dep, Acc)->
    FilePath = fs_path(Dep),
    case file:read_file(FilePath) of
      {ok, DepContent} ->
        Acc ++ [DepContent] ;
      {error, Error} ->
        error_logger:error_msg("~s not found", FilePath),
        Acc
    end
  end, [Content], Deps),
  BinaryContent = iolist_to_binary(lists:reverse(IOListContent)), 
  MaybeMin = maybe_minify(Type, BinaryContent),
  Hash = hash(MaybeMin),
  ets:insert_new(boss_assets, {Asset, Hash, Deps, MaybeMin}),
  {Asset, Hash, Deps, MaybeMin}.

get_served_filename(Filename, Hash) ->
  [Ext | Rest ] = lists:reverse(string:tokens(Filename, ".")),
  Name = string:join(lists:reverse(Rest), "."),  
  lists:flatten([Name, $., Hash, $., Ext]).

should(Option)->
  App = boss_env:get_env(asset_pipeline, assets_for, asset_pipeline),
  case boss_env:get_env(asset_pipeline, Option, production) of
    production ->
      not boss_env:is_developing_app(App);
    Boolean ->
      Boolean
  end.

hash(Content)->
  <<X:128/big-unsigned-integer>> = erlang:md5(Content),
  lists:flatten(io_lib:format("~32.16.0b", [X])).


maybe_minify(javascript, Content)->
  case should(minify) of 
    true ->
      minifier:min_js(Content); 
    false ->
      Content
  end;

maybe_minify(_, Content) ->
  Content.

get_deps(javascript, Contents)->
  {ok, MP} = re:compile("//[[:space:]]*require[[:space:]]*(.*)", []),
  case re:run(Contents, MP, [{capture, all_but_first, list}, global]) of
    nomatch ->
      [];
    {match, List} ->
      [Filename || [Filename] <- List]
  end;

get_deps(_, Contents) -> [].


file_to_type(Filename)->
  case tl(string:tokens(Filename, ".")) of
    ["js"] ->
      javascript;
    ["css"] ->
      stylesheet
  end.
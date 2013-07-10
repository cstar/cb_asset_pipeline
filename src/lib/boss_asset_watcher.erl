-module (boss_asset_watcher).

-behaviour(gen_server).

-record(file_info,{size, type, access, atime, mtime, ctime, mode, links, major_device, minor_device, inode, uid, gid}).



-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

-define(INTERVAL, 1000).

start_link() -> 
  gen_server:start_link(?MODULE, [], []).


init([]) -> 
  timer:send_after(?INTERVAL, watch), 
  {ok, dict:new()}.


handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(watch, Dict) -> 
  Assets = boss_assets:list(),
  {Dict2, ShouldReload} = lists:foldl(
    fun(File, {OldDict, SR})->
      {ok, FileInfo} = file:read_file_info(File, [{time, posix}]),
      Mtime = FileInfo#file_info.mtime,
      case dict:find(File, OldDict) of
        {ok, Value} ->
          {dict:store(File, Value, OldDict), SR orelse Value < Mtime};
        error ->
          {dict:store(File, Mtime, OldDict), SR}
      end
    end, {Dict, false}, Assets),
  maybe_clean(ShouldReload),
  timer:send_after(?INTERVAL, watch), 
  {noreply, Dict2}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, Extra) -> {ok, State}.

maybe_clean(false) -> ok;
maybe_clean(true) -> 
  error_logger:info_msg("Assets have changed. Killing all assets"),
  boss_assets:clean().

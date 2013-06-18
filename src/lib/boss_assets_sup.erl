-module (boss_assets_sup).

-behavior(supervisor).

-export([start_link/0, asset_proc/1, kill/0, files_to_monitor/0]).
-export([init/1]).

start_link()->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

kill() ->
  [ { supervisor:terminate_child(?MODULE, Id),
      supervisor:delete_child(?MODULE, Id) }
    || {Id, _, _, _ } <- supervisor:which_children(?MODULE) ].

asset_proc(File)->
  Children = supervisor:which_children(?MODULE),
  case lists:keyfind(File, 1, Children) of
    false ->
      Spec = {File,
        {boss_asset_file, start_link, [File]}, 
        transient, 5000, worker, dynamic},
      case supervisor:start_child(?MODULE, Spec) of
        {ok, Pid} ->
          Pid;
        Error ->
          error_logger:error_msg("Asset ~p error : ~p ", [File, Error]),
          not_found
        end;
    {File, Pid, _, _} ->
      Pid
  end.

files_to_monitor()->
  [boss_asset_file:path(Pid) || {Id , Pid, _, _ } <- supervisor:which_children(?MODULE), Id =/= boss_asset_watcher ].


init([])->
  Watcher = {boss_asset_watcher,
    {boss_asset_watcher, start_link, []}, 
     permanent, 5000, worker, dynamic},
  {ok, {{one_for_one, 1, 60}, [Watcher]}}.
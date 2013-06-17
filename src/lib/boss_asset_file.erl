-module (boss_asset_file).

-behaviour(gen_server).
-export([start_link/1]).

-export([contents/1, urls/2]).

-export([get_deps/2, test/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

-record (state, {content, filename, type, compressed, deps=[]}).

start_link(File) -> 
  gen_server:start_link(?MODULE, [File], []).

init([File]) -> 
  Type = file_to_type(File),
  Filename = filename:join([boss_files:root_priv_dir(asset_pipeline), "assets", Type, File]),
  case file:read_file(Filename) of
    {ok, Content} ->
      {ok, #state{type = Type, filename = File, content = Content, deps = get_deps(Type, Content)}};
    {error, Error} ->
      {stop, Error}
    end.

urls(Pid, Development)->
  gen_server:call(Pid, {urls, Development}).

contents(Pid)->
  gen_server:call(Pid, content).

handle_call(content, _From, State = #state{content = Content, deps = Deps})->
  %AssetContent = lists:map(fun(Dep)->
  %  Pid = boss_assets_sup:asset_proc(Dep), 
  %  contents(Pid)
  %end, Deps) ++ [Content],
  AssetContent = Content,
  {reply, {ok, AssetContent, []}, State };

handle_call({urls, Development}, _From, State = #state{deps = Deps, filename = Filename, type=Type})->
  Filenames = Deps ++ [Filename],
  {reply, {ok, Filenames}, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, Extra) -> {ok, State}.

file_to_type(Filename)->
  case tl(string:tokens(Filename, ".")) of
    ["js"] ->
      "javascript";
    ["css"] ->
      "stylesheet"
  end.

get_deps("javascript", Contents)->
  {ok, MP} = re:compile("//[[:space:]]*require[[:space:]]*(.*)", []),
  case re:run(Contents, MP, [{capture, all_but_first, list}, global]) of
    nomatch ->
      [];
    {match, List} ->
      [Filename || [Filename] <- List]
  end.

test()->
  Filename = filename:join([boss_files:root_priv_dir(asset_pipeline), "assets", "javascript", "application.js"]),
  {ok, Contents} = file:read_file(Filename),
  get_deps("javascript",Contents).


%% jsmin in Erlang
%% <http://javascript.crockford.com/jsmin.html>
%% NOTE: The production version of JQuery.min.js is actually compressed/altered,
%% (by hand?) and includes http://sizzlejs.com/ - so this function won't generate a 
%% duplicate of the JQuery minified delivery. It will, however, duplicate the result
%% of using: jsmin <jquery-{version}.js >jquery-{version}.min.js

%% Replace // comments with LF
min_js([$/, $/|T], Acc) ->
  Rest = skip_to($\n, T),
  min_js([$\n|Rest], Acc);
%% Replace /* */ comments with a space
min_js([$/, $*|T], Acc) ->
  Rest = skip_to([$*, $/], T),
  min_js([$ |Rest], Acc);
%% Trap regex
min_js([$/|T], [Prev|Acc]) ->
  {Rest, Acc1} = 
    case is_js_regex(Prev) of
    true -> read_to($/, T, [$/, Prev|Acc]);
    false -> {T, [$/, Prev|Acc]}
    end,
  min_js(Rest, Acc1);
%% Trap double quoted strings...
min_js([$"|T], Acc) ->
  {Rest, Acc1} = read_to($", T, [$"|Acc]),
  min_js(Rest, Acc1);
%% Trap single-quoted strings...
min_js([$'|T], Acc) ->
  {Rest, Acc1} = read_to($', T, [$'|Acc]),
  min_js(Rest, Acc1);
%% Replace CR with LF
min_js([$\r|T], Acc) ->
  min_js([$\n|T], Acc);
%% Replace ctrl chars except LF, (but including TAB) with a space
%% NOTE: Assumes "ctrl chars" for ASCII cover all control chars
min_js([H|T], Acc) when H =:= 127 
    orelse (H < 32 andalso H =/= 10) -> 
  min_js([$ |T], Acc);
%% Reduce runs of spaces to one space
min_js([$ |T], Acc = [$ |_]) ->
  min_js(T, Acc);
%% Reduce runs of LF to one LF
min_js([$\n|T], Acc = [$\n|_]) ->
  min_js(T, Acc); 
%% Pre-Collapse whitespace
min_js([$\n, $ |T], Acc) ->
  min_js([$\n|T], Acc);
min_js([$\n, $\t|T], Acc) ->
  min_js([$\n|T], Acc);
min_js([$\n, $\r|T], Acc) ->
  min_js([$\n|T], Acc);
%% For compliance with Cockroft's jsmin.c implementation, trim any leading SPACE
min_js([$ |T], []) ->
  min_js(T, []);
%% For compliance with Cockroft's jsmin.c implementation, trim the trailing LF
min_js([$\n], Acc) ->
  min_js([], Acc);
%% Drop space when permissable
min_js([$ , Next|T], [Prev|Acc]) ->
  case is_omit_unsafe(Prev, $ , Next) of
  true -> min_js([Next|T], [$ , Prev|Acc]);
  false -> min_js([Next|T], [Prev|Acc])
  end;
%% Drop LF when permissable
min_js([$\n, Next|T], [Prev|Acc]) ->
  case is_omit_unsafe(Prev, $\n, Next) of
  true -> min_js([Next|T], [$\n, Prev|Acc]);
  false -> min_js([Next|T], [Prev|Acc])
  end;
%% Don't touch anything else
min_js([H|T], Acc) ->
  min_js(T, [H|Acc]);
min_js([], Acc) ->
  lists:reverse(Acc).

% found terminal char, return
skip_to(X, [X|T]) -> 
  T;
% found terminal chars, return
skip_to([X, Y], [X, Y|T]) -> 
  T;
% pass over everything else
skip_to(Match, [_H|T]) -> 
  skip_to(Match, T);
% error
skip_to(_, []) -> 
  throw("Unterminated Comment").

%% trap escapes
read_to(X, [$\\, H|T], Acc) -> 
  read_to(X, T, [H, $\\|Acc]);
% found terminal char, return
read_to(X, [X|T], Acc) -> 
  {T, [X|Acc]};
% pass through everything else
read_to(X, [H|T], Acc) -> 
  read_to(X, T, [H|Acc]);
% error
read_to(_, [], _Acc) -> 
  throw("Unterminated String").

%% Found / when previous non-ws char is one of:
%% ( ,  =  :  [  !  &  |  ?  {  }  ;  \n
is_js_regex(Prev) ->
  case re:run(<<Prev>>, "[\(,=:\[!&\|\?{};\n]") of
  {match, _} -> true;
  nomatch -> false
  end.

%% jsmin Spec: Omit space except when it is preceded and followed by a non-ASCII character 
%% or by an ASCII letter or digit, or by one of these characters: \ $ _
is_omit_unsafe(Prev, $ , Next) ->
  Regex = "[A-Za-z0-9_\\\\$]",
  is_match(Next, Regex) 
  andalso is_match(Prev, Regex);
%% jsmin Spec: Omit linefeed except:
%% if it follows a non-ASCII character or an ASCII letter or digit 
%% or one of these characters:  \ $ _ } ] ) + - " '
%% AND if it precedes a non-ASCII character or an ASCII letter or digit 
%% or one of these characters:  \ $ _ { [ ( + -
is_omit_unsafe(Prev, $\n, Next) ->
  (Prev =:= $" orelse Prev =:= $' 
    orelse is_match(Prev, "[A-Za-z0-9\\\\$_}\\]\)\+-]")) 
  andalso is_match(Next, "[A-Za-z0-9\\\\\$_{\[\(\+-]").
%%
is_match(X, Regex) ->
  case re:run(<<X>>, Regex) of
  {match, _} -> true;
  nomatch when X >= 128 -> true; % include non-ascii chars
  nomatch -> false
  end.

-module(stylish).
-export([compile/1]).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    stylish_sup:start_link().

stop(_State) ->
    ok.


compile(FilePath) ->
  FilePathString = case is_binary(FilePath) of
    true -> binary_to_list(FilePath);
    _ -> FilePath
  end,

  os:cmd( string:join([command_path(), FilePathString], " ") ).


command_path() ->
    PrivDir = case code:priv_dir(?MODULE) of
                {error, bad_name} ->
                  EbinDir = filename:dirname(code:which(?MODULE)),
                  AppPath = filename:dirname(EbinDir),
                  filename:join(AppPath, "priv");
                Path ->
                  Path
              end,
    filename:join([PrivDir, "sassc", "bin", "sassc"]).

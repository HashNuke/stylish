-module(sassc).
-export([compile/1]).


compile(FilePath)->
  os:cmd( string:join([command_path(), FilePath], " ") ).


command_path() ->
    PrivDir = case code:priv_dir(?MODULE) of
                {error, bad_name} ->
                  EbinDir = filename:dirname(code:which(?MODULE)),
                  AppPath = filename:dirname(EbinDir),
                  filename:join(AppPath, "priv");
                Path ->
                  Path
              end,
    spawn(?MODULE, init, [filename:join([PrivDir, "sassc", "bin", "sassc"])]).

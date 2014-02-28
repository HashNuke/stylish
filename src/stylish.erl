-module(stylish).
-export([compile/1, compile/2]).

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
  compile(FilePath, []).


compile(FilePath, Options) ->
  FormattedOptions = format_options(Options),
  FilePathString = case is_binary(FilePath) of
    true -> binary_to_list(FilePath);
    _ -> FilePath
  end,

  CommandParts = [command_path(), FilePathString] ++ [FormattedOptions],
  os:cmd( string:join(CommandParts, " ") ).



format_options(Options)->
  LineNumbers = get_line_numbers_option(Options),
  SourceMap = get_source_map_option(Options),
  LoadPaths = get_load_path_options(Options),
  Output = get_output_path_option(Options),

  string:join([LineNumbers, SourceMap, LoadPaths, Output], " ").


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

get_load_path_options(Options)->
  Paths = proplists:get_value("load_paths", Options, []),
  PathOptionMapper = fun(Path)->
    case is_binary(Path) of
      false ->
        "-I " ++ " " ++ Path;
      _ ->
        binary_to_list("-I " ++ " " ++ Path)
    end
  end,

  CleanPathOptions = lists:map(PathOptionMapper, Paths),
  string:join([CleanPathOptions], " ").


get_line_numbers_option(Options)->
  case proplists:get_value("line_numbers", Options) of
    true -> "-l";
    _ -> ""
  end.


get_source_map_option(Options)->
  case proplists:get_value("source_map", Options) of
    true ->
      case proplists:get_value("output", Options) of
        undefined -> "";
        _ -> "-m"
      end;
    _ -> ""
  end.


get_output_path_option(Options)->
  Output = proplists:get_value("output", Options),
  case Output of
    undefined -> "";
    _ -> Output
  end.
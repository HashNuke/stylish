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
  run_command(string:join(CommandParts, " ")).


run_command(Command)->
  case exec:run(Command, [stdout, stderr, sync]) of
    {ok, Rssponse} ->
      {ok, proplists:get_value(stdout, Response, "")}.
    {error, Response} ->
      {error, proplists:get_value(stderr, Response, "")}.
  end.


format_options(Options)->
  LineNumbers = get_line_numbers_option(Options),
  SourceMap = get_source_map_option(Options),
  LoadPaths = get_load_path_options(Options),
  Output = get_output_path_option(Options),
  Style = get_style_option(Options),

  string:join([LineNumbers, Style, SourceMap, LoadPaths, Output], " ").


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
  Paths = proplists:get_value('load_paths', Options, []),
  PathOptionMapper = fun(Path)->
    "-I " ++ " " ++ format_option_value(Path)
  end,

  CleanPathOptions = lists:map(PathOptionMapper, Paths),
  string:join([CleanPathOptions], " ").


get_line_numbers_option(Options)->
  case proplists:get_value('line_numbers', Options) of
    true -> "-l";
    _ -> ""
  end.


get_style_option(Options)->
  OptionValue = proplists:get_value('style', Options, "nested"),
  FormattedValue = format_option_value(OptionValue),
  "-t" ++ " " ++ FormattedValue.


get_source_map_option(Options)->
  case proplists:get_value('source_map', Options) of
    true ->
      case proplists:get_value('output', Options) of
        undefined -> "";
        _ -> "-m"
      end;
    _ -> ""
  end.


get_output_path_option(Options)->
  Output = proplists:get_value('output', Options),
  case Output of
    undefined -> "";
    _ -> format_option_value(Output)
  end.


format_option_value(Value)->
  case is_binary(Value) of
    true -> binary_to_list(Value);
    _ -> Value
  end.

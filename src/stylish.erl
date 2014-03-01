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
  FileOutput = get_output_path_option(Options),
  CommandParts = [command_path(), binary_to_list(FilePath)] ++ [FormattedOptions],
  Command = string:join(CommandParts, " "),
  run_command(Command, FileOutput).


run_command(Command, FileOutput)->
  case exec:run(Command, [stdout, stderr, sync]) of
    {ok, Response} ->
      case FileOutput of
        undefined ->
          {ok, hd(proplists:get_value(stdout, Response, [""])) };
        _ -> ok
      end;
    {error, Response} ->
      {error, hd(proplists:get_value(stderr, Response, "")) }
  end.


format_options(Options)->
  LineNumbers = get_line_numbers_option(Options),
  SourceMap = get_source_map_option(Options),
  LoadPaths = get_load_path_options(Options),
  Output = get_output_path_option(Options),
  Style = get_style_option(Options),

  ValidOptions = lists:filter(fun(Option)->
      case Option of
        undefined -> false;
        [] -> false;
        _ -> true
      end
    end, [LineNumbers, Style, SourceMap, LoadPaths, Output]),

  string:join(ValidOptions, " ").


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
  Paths = proplists:get_value(<<"load_paths">>, Options, []),
  PathOptionMapper = fun(Path)->
    "-I " ++ " " ++ binary_to_list(Path)
  end,

  CleanPathOptions = lists:map(PathOptionMapper, Paths),
  string:join(CleanPathOptions, " ").


get_line_numbers_option(Options)->
  case proplists:get_value(<<"line_numbers">>, Options) of
    true -> "-l";
    _ -> undefined
  end.


get_style_option(Options)->
  case proplists:get_value(<<"compress">>, Options, false) of
    true  -> "-t nested";
    false -> "-t compressed"
  end.


get_source_map_option(Options)->
  case proplists:get_value(<<"source_map">>, Options) of
    true ->
      case proplists:get_value(<<"output">>, Options) of
        undefined -> undefined;
        _ -> "-m"
      end;
    _ -> undefined
  end.


get_output_path_option(Options)->
  Output = proplists:get_value(<<"output">>, Options),
  case Output of
    undefined -> undefined;
    _ -> binary_to_list(Output)
  end.

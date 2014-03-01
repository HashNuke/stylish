# stylish

> Erlang library to compile SCSS files.

Uses the [libsass](https://github.com/hcatlin/libsass) project.

## Usage

Add `stylish` to your rebar deps (or mix deps if you are running Elixir).


```
% with default options
stylish:compile(<<"path/to/anything.scss">>)

% with options
stylish:compile(<<>>"path/to/anything.scss">>, [<<"style">>, <<"compressed">>])
```

#### Return values

* For string output, if compilation is a success: `{ok, CompiledCss}`

* For file output (file path passed to `output` option), if compilation is a success: `ok`

* If there is a compile error, then `{error, Error}`

#### Options

The following options can be passed in the second argument. All options must be binaries

* `line_numbers` - `false` (default) or `true`. Displays line numbers.

* `compress` - `false` (default) or true.

* `load_paths` - an array of paths to use as load paths for SASS imports. All paths must be erlang binaries.

* `output` - If you want the resulting CSS written to a file, then pass a path (as a binary). By default a string is returned.

* `source_map` - `false` (default) or `true`. Generates source maps if `output` option is set.

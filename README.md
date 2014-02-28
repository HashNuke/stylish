# stylish

> Erlang wrapper to compile sass files

### Usage

Add `stylish` to your rebar deps (or mix deps if you are running Elixir).

```
stylish:compile("path/to/anything.scss")
```

The following options can be passed as a second argument:

* `line_numbers` - `false` (default) or `true`. Displays line numbers.

* `style` - `nested` (default) or `compressed`.

* `load_paths` - an array of paths to use as load paths for SASS imports.

* `output` - If you want the resulting CSS written to a file, then pass a path. By default a string is returned.

* `source_map` - Generates source maps if output file path is set.

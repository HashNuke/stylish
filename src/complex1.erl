-module(complex1).
-export([start/0, stop/0, init/1]).
-export([foo/1, bar/1]).

start() ->
    PrivDir = case code:priv_dir(?MODULE) of
                {error, bad_name} ->
                  EbinDir = filename:dirname(code:which(?MODULE)),
                  AppPath = filename:dirname(EbinDir),
                  filename:join(AppPath, "priv");
                Path ->
                  Path
              end,
    spawn(?MODULE, init, [filename:join([PrivDir, "stylish"])]).
stop() ->
    complex ! stop.

foo(X) ->
    call_port({foo, X}).
bar(Y) ->
    call_port({bar, Y}).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
    	{complex, Result} ->
        Result
    end.

init(ExtPrg) ->
  register(complex, self()),
  process_flag(trap_exit, true),
  Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary]),
  loop(Port).

loop(Port) ->
  receive
    {call, Caller, Msg} ->
      Port ! {self(), {command, term_to_binary(Msg)}},
      receive
        {Port, {data, Data}} ->
          Caller ! {complex, binary_to_term(Data)}
      end,
	    loop(Port);
    stop ->
	    Port ! {self(), close},
	    receive
    		{Port, closed} ->
          exit(normal)
	    end;
    {'EXIT', Port, Reason} ->
      exit(port_terminated)
  end.

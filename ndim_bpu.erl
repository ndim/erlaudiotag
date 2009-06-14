%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ndim's binary parsing utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(ndim_bpu).


-export([dump_bytes/2]).
-export([msleep/1]).


-define(BYTE_COUNT, 64).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Debugging Aids
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


msleep(MSec) ->
    receive
    after MSec ->
	    ok
    end.


first_bytes(Bin) when is_binary(Bin) ->
    {FirstBytes, _} = split_binary(Bin, ?BYTE_COUNT),
    FirstBytes.


last_bytes(Bin) when is_binary(Bin) ->
    {_, LastBytes} = split_binary(Bin, size(Bin) - ?BYTE_COUNT),
    LastBytes.


shorten_bytes(Bin) when is_binary(Bin), size(Bin) > 2*?BYTE_COUNT ->
    {first_bytes(Bin), last_bytes(Bin)};
shorten_bytes(Bin) when is_binary(Bin) ->
    Bin.


dump_bytes(Bin, Msg) when is_binary(Bin) ->
    case Msg of
	none -> io:format("  Byte dump:~n", []);
	_ ->    io:format("  Byte dump (~s):~n", [Msg])
    end,
    io:format("    Size: ~w~n", [size(Bin)]),
    case shorten_bytes(Bin) of
	{F,L} ->
	    io:format("    Data: ~p~n"
		      "          ...~n"
		      "          ~p~n",
		      [F,L]);
	Bin ->
	    io:format("    Data: ~p~n", [Bin])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ndim_bpu.erl - ndim's binary parsing utilities
%% Copyright (C) 2009 Hans Ulrich Niedermann
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(ndim_bpu).


-export([dump_bytes/2]).
-export([msleep/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

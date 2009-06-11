%%
%% http://www.id3.org/id3v2.4.0-structure

-module(id3parse).

-export([test/1, test/0]).
-export([parse_data/1]).

parse_data(Data, Acc) ->
    parse_header(Data, Acc).

parse_header(<<
	      "ID3",
	      VerMajor, VerMinor,
	      Flags:1/binary,
	      Rest/binary
	      >>,
	     Acc) ->
    <<
     FlagUnsync:1,
     FlagExtendedHeader:1,
     FlagExperimental:1,
     FlagFooter:1,
     0:1,
     0:1,
     0:1,
     0:1
     >> = Flags,
    io:format("ID3 header: "
	      "ver:id3v2.~w.~w "
	      "flags:~w ( ~s~s~s~s)"
	      "~n",
	      [
	       VerMajor, VerMinor,
	       Flags,
	       case FlagUnsync of
		   1 -> "unsync ";
		   0 -> ""
	       end,
	       case FlagExtendedHeader of
		   1 -> "exthdr ";
		   0 -> ""
	       end,
	       case FlagExperimental of
		   1 -> "exphdr ";
		   0 -> ""
	       end,
	       case FlagFooter of
		   1 -> "footer ";
		   0 -> ""
	       end
	      ]),
    Hdr = {header, {id3v2version, VerMajor, VerMinor}},
    R1 = case FlagExtendedHeader of
	     1 -> parse_extended_header(Rest, [Hdr|Acc]);
	     0 -> parse

parse_data(Data) ->
    parse_data(Data, []).


test_item(FileName) ->
    {ok, Data} = file:read_file(FileName),
    parse_data(Data).

test([], Acc) ->
    lists:reverse(Acc);
test([Head|Tail], Acc) ->
    test(Tail, [test_item(Head)|Acc]).

test(List) when is_list(List) ->
    test(List, []).

test() ->
    test([]).

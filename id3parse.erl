%% Links:
%% http://www.id3.org/
%% http://www.id3.org/Developer_Information
%% http://www.id3.org/id3v2.4.0-structure
%% http://www.id3.org/id3v2.4.0-frames


-module(id3parse).


-export([test/1, test/0]).
-export([parse_data/1]).


-define(BYTE_COUNT, 64).


parse_data(Data, Acc) when is_binary(Data), is_list(Acc) ->
    parse_header(Data, Acc).


parse_header(<<
	      "ID3",
	      VerMajor, VerMinor,
	      Flags:1/binary,
	      Size:4/binary,
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
    {Unsync, FlagList0} = case FlagUnsync of
			      1 -> {true,  [unsync]};
			      0 -> {false, []}
			  end,
    FlagList1 = case FlagExtendedHeader of
		    1 -> [ext_hdr|FlagList0];
		    0 -> FlagList0
		end,
    FlagList2 = case FlagExperimental of
		    1 -> [experimental|FlagList1];
		    0 -> FlagList1
		end,
    FlagList = case FlagFooter of
		   1 -> [footer|FlagList2];
		   0 -> FlagList2
	       end,
    RealSize = unsynch(Unsync, Size),
    io:format("ID3 header: "
	      "ver:id3v2.~w.~w "
	      "flags:~w (~w) "
	      "size:~w"
	      "~n",
	      [
	       VerMajor, VerMinor,
	       Flags, FlagList,
	       RealSize
	      ]),
    Hdr = {header, [{id3v2version, VerMajor, VerMinor},
		    {flags, FlagList},
		    {real_size, RealSize}]},
    R1 = case FlagExtendedHeader of
	     1 -> parse_extended_header(Unsync, Rest, [Hdr|Acc]);
	     0 -> parse_frame(Unsync, Rest, [Hdr|Acc])
	 end,
    R1.


parse_extended_header(Unsync,
		      <<
		       ExtHdrSize:32/integer,
		       ExtHdrStuff:ExtHdrSize/binary,
		       Rest/binary
		       >>, Acc) ->
    <<
     FlagByteCount:8/integer,
     ExtendedFlags:FlagByteCount/binary
     >> = ExtHdrStuff,
    parse_frame(Unsync,
		Rest,
		[{extended_header,
		  ExtHdrSize, FlagByteCount,
		  ExtendedFlags}|Acc]).


unsynch(true,
	  <<
	   0:1, S3:7,
	   0:1, S2:7,
	   0:1, S1:7,
	   0:1, S0:7
	   >>) ->
    (((((S3 bsl 7) + S2) bsl 7) + S1) bsl 7) + S0;
unsynch(false, <<S:32/integer>>) ->
    S.


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
	    io:format("    Data: ~p~s~n", [Bin])
    end.

dump_bytes(Bin) when is_binary(Bin) ->
    dump_bytes(Bin, none).


parse_frame(Unsync,
	    <<0,0,0,0, Rest/binary>>, Acc) ->
    parse_frame(Unsync, Rest, Acc);
parse_frame(Unsync,
	    <<
	     FrameID:4/binary,
	     FrameSize:4/binary,
	     FrameFlags:2/binary,
	     Rest/binary
	     >>, Acc) ->
    RealSize = unsynch(Unsync, FrameSize),
    io:format("frame id    (raw): ~w (~s)~n"
	      "frame size  (raw): ~w (~w)~n"
	      "frame flags (raw): ~w~n",
	      [FrameID, binary_to_list(FrameID),
	       FrameSize, RealSize,
	       FrameFlags]),
    dump_bytes(Rest, "Frame data"),
    <<
     0:1,
     FrameTagAlterDiscard:1,
     FrameFileAlterDiscard:1,
     FrameReadOnly:1,

     0:1, 0:1, 0:1, 0:1,

     0:1,
     FrameGroupingIdentity:1,
     0:1,
     0:1,

     FrameCompressed:1,
     FrameEncrypted:1,
     FrameUnsync:1,
     FrameDataLengthIndicator:1

     >> = FrameFlags,

    io:format("parse_frame id=\"~s\" size=~w=~w flags=~w~n",
	      [binary_to_list(FrameID), FrameSize,
	       RealSize, FrameFlags]),

    <<
     Data:RealSize/binary,
     NewRest/binary
     >> = Rest,

    parse_frame_int(Unsync, FrameID, RealSize, FrameFlags, Data, NewRest, Acc).


text_content_int(<<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
text_content_int(<<0, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
text_content_int(<<Char, Rest/binary>>, Acc) ->
    text_content_int(Rest, [Char|Acc]).

text_content_int(Text) when is_binary(Text) ->
    text_content_int(Text, []).


text_content(<<>>, Acc) ->
    lists:reverse(Acc);
text_content(Text, Acc) ->
    {String, NewText} = text_content_int(Text),
    text_content(NewText, [String|Acc]).

text_content(Text) when is_binary(Text) ->
    text_content(Text, []).


parse_frame_int(Unsync, <<"TXXX">> = FrameID, FrameSize, FrameFlags, Data, Rest, Acc) ->
    parse_frame(Unsync, Rest, [{txxx_frame, FrameID, FrameSize, FrameFlags, Data}|Acc]);
parse_frame_int(Unsync,
		<<"T",T1:8/integer,T2:8/integer,T3:8/integer>> = FrameID,
		FrameSize, FrameFlags,
		<<TextEncoding:8/integer, Text/binary>> = Data,
		Rest, Acc)
  when ($0 =< T1), (T1 =< $Z),
       ($0 =< T2), (T2 =< $Z),
       ($0 =< T3), (T3 =< $Z)
       ->
    io:format("  Text tag: ~s~n"
	      "  Encoding: ~w~n",
	      [binary_to_list(FrameID),
	       TextEncoding]),
    io:format("   Content: ~p~n", [text_content(Text)]),
    parse_frame(Unsync, Rest, [{text_frame, FrameID, FrameSize, FrameFlags, Data}|Acc]);
parse_frame_int(Unsync, <<"APIC">> = FrameID,
		FrameSize, FrameFlags,
		<<
		 TextEncoding:8/integer,
		 Data/binary
		 >>,
		Rest, Acc) ->
    dump_bytes(Data, "Data"),
    {MimeType, Data1} = text_content_int(Data),
    dump_bytes(Data1, "Data1"),
    <<PictureType:8/integer, Data2/binary>> = Data1,
    dump_bytes(Data2, "Data2"),
    {Description, ImgData} = text_content_int(Data2),
    dump_bytes(ImgData, "ImgData"),
    dump_bytes(Rest, "Rest"),
    io:format("  APIC tag:      ~s~n"
	      "    TextEnc:     ~w~n"
	      "    MIMEType:    ~s~n"
	      "    PictureType: ~w~n",
	      [binary_to_list(FrameID),
	       TextEncoding,
	       MimeType,
	       PictureType
	      ]),
    parse_frame(Unsync, Rest,
		[{apic_frame, FrameID, FrameSize, FrameFlags, Data}|Acc]);
parse_frame_int(Unsync,
		<<T0:8/integer,T1:8/integer,T2:8/integer,T3:8/integer>>=FrameID,
		FrameSize, FrameFlags, Data, Rest, Acc)
  when ($0 =< T0), (T0 =< $Z),
       ($0 =< T1), (T1 =< $Z),
       ($0 =< T2), (T2 =< $Z),
       ($0 =< T3), (T3 =< $Z)
       ->
    parse_frame(Unsync, Rest,
		[{generic_frame, FrameID, FrameSize, FrameFlags, Data}|Acc]).


parse_footer(<<
	      "3DI",
	      VerMajor, VerMinor,
	      Flags:1/binary,
	      Rest/binary
	      >>, Acc) ->
    Acc.


parse_data(Data) ->
    parse_data(Data, []).


test_item(FileName) ->
    {ok, Data} = file:read_file(FileName),
    P = parse_data(Data),
    io:format("~s:~n  ~p~n", [FileName, P]),
    P.


test([], Acc) ->
    lists:reverse(Acc);
test([FileName|Tail], Acc) ->
    test(Tail, [test_item(FileName)|Acc]).


test(List) when is_list(List) ->
    test(List, []).


test() ->
    test([]).

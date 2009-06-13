%% Links:
%% http://www.id3.org/
%% http://www.id3.org/Developer_Information
%% http://www.id3.org/id3v2.4.0-structure
%% http://www.id3.org/id3v2.4.0-frames


-module(id3parse).


-export([test/1, test/0]).
-export([parse_data/1]).


-define(BYTE_COUNT, 64).


-record(id3v2_tag,
	{size, version, flags, ext_hdr=none, frames=[], footer=none}).

-record(id3v2_version,
	{major, minor}).

-record(id3v2_tag_flags,
	{unsynch=false,
	 ext_hdr=false,
	 experimental=false,
	 footer=false
	}).

-record(id3v2_frame_flags,
	{tag_alter_discard=false,
	 file_alter_discard=false,
	 read_only=false,
	 grouping_identity=false,
	 compressed=false,
	 encrypted=false,
	 unsynch=false,
	 data_length_indicator=false
	}).

-record(id3v2_frame,
	{type, size, flags,
	 data}).

-record(id3v2_text_frame,
	{type, size, flags,
	 text_encoding, text}).

-record(id3v2_apic_frame,
	{type, size, flags,
	 text_encoding, description,
	 mime_type, pic_type, imgdata}).


parse_data(Data, Acc) when is_binary(Data), is_list(Acc) ->
    parse_header(Data, Acc).


int_to_bool(0) ->
    false;
int_to_bool(1) ->
    true.


parse_tag_flags(<<
		 FlagUnsync:1,
		 FlagExtendedHeader:1,
		 FlagExperimental:1,
		 FlagFooter:1,
		 0:1,
		 0:1,
		 0:1,
		 0:1
		 >>) ->
    #id3v2_tag_flags{unsynch=int_to_bool(FlagUnsync),
		     ext_hdr=int_to_bool(FlagExtendedHeader),
		     experimental=int_to_bool(FlagExperimental),
		     footer=int_to_bool(FlagFooter)}.


parse_header(<<
	      "ID3",
	      VerMajor, VerMinor,
	      Flags:1/binary,
	      Size:4/binary,
	      Rest/binary
	      >>,
	     Acc) ->
    TagFlags = parse_tag_flags(Flags),
    RealSize = unsynch(TagFlags#id3v2_tag_flags.unsynch, Size),
    io:format("ID3 header: "
	      "ver:id3v2.~w.~w "
	      "flags:~w (~w) "
	      "size:~w"
	      "~n",
	      [
	       VerMajor, VerMinor,
	       Flags, TagFlags,
	       RealSize
	      ]),
    Hdr = #id3v2_tag{version=#id3v2_version{major=VerMajor, minor=VerMinor},
		     flags=TagFlags,
		     size=RealSize},
    parse_extended_header(TagFlags, Rest, [Hdr|Acc]).


parse_extended_header(#id3v2_tag_flags{ext_hdr=false} = TagFlags, Rest, Acc) ->
    parse_frame(TagFlags, Rest, [Acc]);
parse_extended_header(TagFlags,
		      <<
		       ExtHdrSize:32/integer,
		       ExtHdrStuff:ExtHdrSize/binary,
		       Rest/binary
		       >>, Acc) ->
    <<
     FlagByteCount:8/integer,
     ExtendedFlags:FlagByteCount/binary
     >> = ExtHdrStuff,
    parse_frame(TagFlags,
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
	    io:format("    Data: ~p~n", [Bin])
    end.


parse_frame_flags(
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
   FrameUnsynch:1,
   FrameDataLengthIndicator:1
   
   >>) ->
    #id3v2_frame_flags
		   {tag_alter_discard=int_to_bool(FrameTagAlterDiscard),
		    file_alter_discard=int_to_bool(FrameFileAlterDiscard),
		    read_only=int_to_bool(FrameReadOnly),
		    grouping_identity=int_to_bool(FrameGroupingIdentity),
		    compressed=int_to_bool(FrameCompressed),
		    encrypted=int_to_bool(FrameEncrypted),
		    unsynch=int_to_bool(FrameUnsynch),
		    data_length_indicator=int_to_bool(FrameDataLengthIndicator)}.


parse_frame(#id3v2_tag_flags{unsynch=Unsync, footer=HasFooter} = TagFlags,
	    <<0,0,0,0, _/binary>> = Rest, Acc) ->
    parse_footer(TagFlags, Rest, Acc);
parse_frame(#id3v2_tag_flags{unsynch=Unsync, footer=HasFooter} = TagFlags,
	    <<
	     FrameID:4/binary,
	     FrameSizeUnsynch:4/binary,
	     FrameFlagsRaw:2/binary,
	     Rest/binary
	     >>, Acc) ->
    FrameSize = unsynch(Unsync, FrameSizeUnsynch),
    io:format("frame id    (raw): ~w (~s)~n"
	      "frame size  (raw): ~w (~w)~n"
	      "frame flags (raw): ~w~n",
	      [FrameID, binary_to_list(FrameID),
	       FrameSizeUnsynch, FrameSize,
	       FrameFlagsRaw]),
    dump_bytes(Rest, "Frame data"),

    FrameFlags = parse_frame_flags(FrameFlagsRaw),

    io:format("parse_frame id=\"~s\" size=~w=~w flags=~w=~w~n",
	      [binary_to_list(FrameID),
	       FrameSizeUnsynch, FrameSize,
	       FrameFlagsRaw, FrameFlags]),

    <<
     Data:FrameSize/binary,
     NewRest/binary
     >> = Rest,

    parse_frame_int(TagFlags, FrameFlags,
		    FrameID, FrameSize, FrameFlags, Data,
		    NewRest, Acc).


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


parse_frame_int(#id3v2_tag_flags{unsynch=Unsync, footer=HasFooter} = TagFlags,
		FrameFlags,
		<<"TXXX">> = FrameID, FrameSize, FrameFlags, Data, Rest, Acc) ->
    parse_frame(TagFlags,
		Rest, [{txxx_frame, FrameID, FrameSize, FrameFlags, Data}|Acc]);
parse_frame_int(#id3v2_tag_flags{unsynch=Unsync, footer=HasFooter} = TagFlags,
		FrameFlags,
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
    parse_frame(TagFlags,
		Rest,
		[#id3v2_text_frame{type=FrameID,
				   size=FrameSize,
				   flags=FrameFlags,
				   text_encoding=TextEncoding,
				   text=Text}|Acc]);
parse_frame_int(#id3v2_tag_flags{unsynch=Unsync, footer=HasFooter} = TagFlags,
		FrameFlags,
		<<"APIC">> = FrameID,
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
	      "    Description: ~w~n"
	      "    PictureType: ~w~n",
	      [binary_to_list(FrameID),
	       TextEncoding,
	       MimeType,
	       Description,
	       PictureType
	      ]),
    parse_frame(TagFlags,
		Rest,
		[#id3v2_apic_frame{type=FrameID,
				   size=FrameSize,
				   flags=FrameFlags,
				   text_encoding=TextEncoding,
				   description=Description,
				   mime_type=MimeType,
				   pic_type=PictureType,
				   imgdata=ImgData}|Acc]);
parse_frame_int(#id3v2_tag_flags{unsynch=Unsync, footer=HasFooter} = TagFlags,
		FrameFlags,
		<<T0:8/integer,T1:8/integer,T2:8/integer,T3:8/integer>>=FrameID,
		FrameSize, FrameFlags, Data, Rest, Acc)
  when ($0 =< T0), (T0 =< $Z),
       ($0 =< T1), (T1 =< $Z),
       ($0 =< T2), (T2 =< $Z),
       ($0 =< T3), (T3 =< $Z)
       ->
    parse_frame(TagFlags,
		Rest,
		[#id3v2_frame{type=FrameID,
			      size=FrameSize,
			      flags=FrameFlags,
			      data=Data}|Acc]).


parse_footer(#id3v2_tag_flags{unsynch=Unsync, footer=HasFooter} = TagFlags,
	    <<0, Rest/binary>>, Acc) ->
    %% skip padding
    parse_footer(TagFlags, Rest, Acc);
parse_footer(#id3v2_tag_flags{unsynch=Unsync, footer=true} = TagFlags,
	     <<
	      "3DI",
	      VerMajor, VerMinor,
	      Flags:1/binary,
	      Rest/binary
	      >>, Acc) ->
    lists:reverse(Acc);
parse_footer(_, <<>>, Acc) ->
    lists:reverse(Acc);
parse_footer(#id3v2_tag_flags{unsynch=Unsync, footer=HasFooter} = TagFlags,
	     Bin, Acc) ->
    dump_bytes(Bin, "parse_footer unhandled"),
    io:format("    Unsync: ~p~n"
	      "    HasFooter: ~p~n",
	      [Unsync, HasFooter]),
    %% io:format("    Acc: ~p~n", [lists:reverse(Acc)]),
    lists:reverse(Acc).


parse_data(Data) ->
    parse_data(Data, []).



render(#id3v2_tag{version=TagVersion,
		  flags=TagFlags,
		  frames=Frames}) ->
    moo.


test_item(FileName) ->
    {ok, Data} = file:read_file(FileName),
    P = parse_data(Data),
    io:format("~s:~n  ~P~n", [FileName, P,length(P)+50]),
    P.


test([], Acc) ->
    lists:reverse(Acc);
test([FileName|Tail], Acc) ->
    test(Tail, [test_item(FileName)|Acc]).


test(List) when is_list(List) ->
    test(List, []).


test() ->
    test([]).

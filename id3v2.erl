%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Links:
%% http://www.id3.org/
%% http://www.id3.org/Developer_Information
%% http://www.id3.org/id3v2.4.0-structure
%% http://www.id3.org/id3v2.4.0-frames
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BUG: Need proper handling of text encodings.
%% BUG: Need to remove data sizes from records where the size is implied.
%% BUG: Need to keep proper track of data sizes while rendering.
%% BUG: Add human readable names of frames.
%% BUG: Verify "unsynch"ed 32bit ints are read and written correctly.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(id3v2).


-export([test/1, test/0]).
-export([parse_data/1, parse_file/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define(TAG_TABLE, id3_frame_ids).
-define(BYTE_COUNT, 64).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse result record definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-record(id3v2_tag,
	{size, version, flags,
	 ext_hdr=none,
	 frames=[],
	 padding=0,
	 footer=none
	}).

-record(id3v2_tag_version,
	{major, minor}).

-record(id3v2_padding,
	{size}).

-record(id3v2_footer,
	{version, flags}).

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
	{id, name, flags,
	 payload}).


-record(id3v2_generic_frame,
	{size, data}).

-record(id3v2_text_frame,
	{size,
	 text_encoding, text}).

-record(id3v2_apic_frame,
	{size,
	 text_encoding, description,
	 mime_type, pic_type, img_data}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0.


int_to_bool(0) ->
    false;
int_to_bool(1) ->
    true.


unsynch_int(<<
	     0:1, S3:7,
	     0:1, S2:7,
	     0:1, S1:7,
	     0:1, S0:7
	     >>) ->
    (((((S3 bsl 7) + S2) bsl 7) + S1) bsl 7) + S0.


unsynch(#id3v2_tag_flags{unsynch=true}, Bin) ->
    unsynch_int(Bin);
unsynch(#id3v2_frame_flags{unsynch=true}, Bin) ->
    unsynch_int(Bin);
unsynch(_, <<S:32/integer>>) ->
    S.


shiftop(Val) ->
    {Val band 16#7f, Val bsr 7}.


ununsynch_int(Val) ->
    {S0, R0} = shiftop(Val),
    {S1, R1} = shiftop(R0),
    {S2, R2} = shiftop(R1),
    {S3, R3} = shiftop(R2),
    0 = R3,
    <<
     0:1, S3:7,
     0:1, S2:7,
     0:1, S1:7,
     0:1, S0:7
     >>.


ununsynch(#id3v2_tag_flags{unsynch=true}, Size) when is_integer(Size) ->
    ununsynch_int(Size);
ununsynch(#id3v2_frame_flags{unsynch=true}, Size) when is_integer(Size) ->
    ununsynch_int(Size);
ununsynch(_, Size) ->
    <<Size:32/integer>>.


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
%% Parse ID3v2 tags from file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


parse_file(FileName) ->
    {ok, Data} = file:read_file(FileName),
    parse_data(Data).


parse_data(Data) when is_binary(Data) ->
    parse_tag(Data).


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


parse_tag(<<
	   "ID3",
	   VerMajor, VerMinor,
	   Flags:1/binary,
	   Size:4/binary,
	   Rest/binary
	   >>) ->
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
    {ExtHdr, Rest1}  = parse_extended_header(TagFlags, Rest),
    {Frames, Rest2}  = parse_frame(TagFlags, Rest1, []),
    {PadSize, Rest3} = skip_padding(Rest2, 0),
    {Footer, Rest4}  = parse_footer(TagFlags, Rest3),
    case Rest4 of
	<<>> -> ok;
	_ -> io:format("CAUTION: Unparsed data at the end of ID3v2 tag!~n", []),
	     dump_bytes(Rest4, "Unparsed data")
    end,
    {#id3v2_tag{version=#id3v2_tag_version{major=VerMajor, minor=VerMinor},
		flags=TagFlags,
		size=RealSize,
		ext_hdr=ExtHdr,
		frames=Frames,
		padding=#id3v2_padding{size=PadSize},
		footer=Footer
	       },
     Rest4}.


parse_extended_header(#id3v2_tag_flags{ext_hdr=false} = _TagFlags, Rest) ->
    {undefined, Rest};
parse_extended_header(_TagFlags,
		      <<
		       ExtHdrSize:32/integer,
		       ExtHdrStuff:ExtHdrSize/binary,
		       Rest/binary
		       >>) ->
    <<
     FlagByteCount:8/integer,
     ExtendedFlags:FlagByteCount/binary
     >> = ExtHdrStuff,
    {{extended_header,
      ExtHdrSize,
      FlagByteCount,
      ExtendedFlags},
     Rest}.


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


parse_frame(#id3v2_tag_flags{unsynch=_Unsync, footer=_HasFooter} = _TagFlags,
	    <<0,0,0,0, _/binary>> = Rest, Acc) ->
    {lists:reverse(Acc), Rest};
parse_frame(#id3v2_tag_flags{unsynch=_Unsync, footer=_HasFooter} = _TagFlags,
	    <<>> = Rest, Acc) ->
    {lists:reverse(Acc), Rest};
parse_frame(#id3v2_tag_flags{unsynch=_Unsync, footer=_HasFooter} = TagFlags,
	    <<
	     FrameID:4/binary,
	     FrameSizeUnsynch:4/binary,
	     FrameFlagsRaw:2/binary,
	     Rest/binary
	     >>, Acc) ->
    FrameSize = unsynch(TagFlags, FrameSizeUnsynch),
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


frameid_atom(FrameID) when is_binary(FrameID) ->
    frameid_atom(binary_to_list(FrameID));
frameid_atom(FrameID) when is_list(FrameID) ->
    list_to_atom(FrameID).


atom_to_frameid(FrameID) when is_atom(FrameID) ->
    list_to_binary(atom_to_list(FrameID)).


parse_frame_int(#id3v2_tag_flags{unsynch=_Unsync, footer=_HasFooter} = TagFlags,
		FrameFlags,
		<<"TXXX">> = FrameID, FrameSize, FrameFlags, Data, Rest, Acc) ->
    parse_frame(TagFlags,
		Rest,
		[#id3v2_frame
		 {id=frameid_atom(FrameID),
		  name=frameid_name(FrameID),
		  flags=FrameFlags,
		  payload=#id3v2_generic_frame
		  {
		    size=FrameSize,
		    data=Data
		   }}|Acc]);
parse_frame_int(#id3v2_tag_flags{unsynch=_Unsync, footer=_HasFooter} = TagFlags,
		FrameFlags,
		<<"T",T1:8/integer,T2:8/integer,T3:8/integer>> = FrameID,
		FrameSize, FrameFlags,
		<<TextEncoding:8/integer, Text/binary>> = _Data,
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
		[#id3v2_frame{id=frameid_atom(FrameID),
			      name=frameid_name(FrameID),
			      flags=FrameFlags,
			      payload=#id3v2_text_frame{
				size=FrameSize,
				text_encoding=TextEncoding,
				text=Text}}|Acc]);
parse_frame_int(#id3v2_tag_flags{unsynch=_Unsync, footer=_HasFooter} = TagFlags,
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
		[#id3v2_frame{id=frameid_atom(FrameID),
			      name=frameid_name(FrameID),
			      flags=FrameFlags,
			      payload=#id3v2_apic_frame{
				size=FrameSize,
				text_encoding=TextEncoding,
				description=Description,
				mime_type=MimeType,
				pic_type=PictureType,
				img_data=ImgData}}|Acc]);
parse_frame_int(#id3v2_tag_flags{unsynch=_Unsync, footer=_HasFooter} = TagFlags,
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
		[#id3v2_frame{id=frameid_atom(FrameID),
			      name=frameid_name(FrameID),
			      flags=FrameFlags,
			      payload=#id3v2_generic_frame{
				size=FrameSize,
				data=Data}}|Acc]).


skip_padding(<<0, Rest/binary>>, Padding) ->
    skip_padding(Rest, Padding+1);
skip_padding(Rest, Padding) when is_binary(Rest) ->
    {Padding, Rest}.


parse_footer(#id3v2_tag_flags{unsynch=_Unsync, footer=true} = _TagFlags,
	     <<
	      "3DI",
	      VerMajor, VerMinor,
	      Flags:1/binary,
	      Rest/binary
	      >>) ->
    {#id3v2_footer{flags=parse_tag_flags(Flags),
		   version=#id3v2_tag_version{major=VerMajor, minor=VerMinor}},
     Rest};
parse_footer(_, <<>>) ->
    {undefined, <<>>};
parse_footer(#id3v2_tag_flags{unsynch=Unsync, footer=HasFooter} = _TagFlags,
	     Bin) ->
    dump_bytes(Bin, "parse_footer unhandled"),
    io:format("    Unsync: ~p~n"
	      "    HasFooter: ~p~n",
	      [Unsync, HasFooter]),
    %% io:format("    Acc: ~p~n", [lists:reverse(Acc)]),
    {undefined, Bin}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rendering ID3v2 tags to file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


render(#id3v2_tag{version=TagVersion,
		  flags=TagFlags,
		  size=OldSize,
		  frames=Frames,
		  padding=Padding,
		  footer=Footer}) ->
    RFrames = render(TagFlags, Frames),
    RPadding = render(TagFlags, Padding),
    RFooter = render(TagFlags, Footer),
    Sizes = [iolist_size(RFrames),
	     iolist_size(RPadding),
	     iolist_size(RFooter)],
    Size = lists:sum(Sizes),
    io:format("render tag: oldsize=~w size=~w (~w)~n",
	      [OldSize, Size, Sizes]),
    [<<"ID3">>,
     render(TagVersion),
     render(TagFlags),
     ununsynch_int(Size), % TagFlags, Size),
     RFrames,
     RPadding,
     RFooter
    ];
render(#id3v2_tag_version{major=Major, minor=Minor}) ->
    <<Major, Minor>>;
render(F) when is_record(F, id3v2_tag_flags)->
    <<
     (bool_to_int(F#id3v2_tag_flags.unsynch)):1,
     (bool_to_int(F#id3v2_tag_flags.ext_hdr)):1,
     (bool_to_int(F#id3v2_tag_flags.experimental)):1,
     (bool_to_int(F#id3v2_tag_flags.footer)):1,
     0:1,
     0:1,
     0:1,
     0:1
     >>.


render(_TagFlags, undefined) ->
    [];
render(_TagFlags, #id3v2_padding{size=0}) ->
    [];
render(_TagFlags, #id3v2_padding{size=Size}) ->
    list_to_binary([ 0 || _ <- lists:seq(1,Size) ]);
render(TagFlags, #id3v2_frame{id=ID, flags=Flags,
			      payload=#id3v2_generic_frame{size=Size,
							   data=Data}}) ->
    [atom_to_frameid(ID),
     ununsynch(TagFlags, Size),
     render(TagFlags, Flags),
     Data];
render(TagFlags, #id3v2_frame{id=ID, flags=Flags,
			      payload=#id3v2_apic_frame{
				size=OldSize,
				text_encoding=TextEncoding,
				description=Description,
				mime_type=MimeType,
				pic_type=PicType,
				img_data=ImgData
			       }}) ->
    Sizes = [1, iolist_size(MimeType), 1,
	     1,
	     iolist_size(Description), 1,
	     size(ImgData)],
    Size = lists:sum(Sizes),
    io:format("render APIC frame: oldsize=~w size=~w (~w)~n",
	      [OldSize, Size, Sizes]),
    [atom_to_frameid(ID),
     ununsynch(TagFlags, Size),
     render(TagFlags, Flags),
     TextEncoding,
     MimeType,0,
     PicType,
     Description,0,
     ImgData];
render(TagFlags, #id3v2_frame{id=ID, flags=Flags,
			      payload=#id3v2_text_frame{
				size=OldSize,
				text_encoding=TextEncoding, text=Text}}) ->
    Sizes = [1, iolist_size(Text)],
    Size = lists:sum(Sizes),
    io:format("render text frame: oldsize=~w size=~w (~w)~n",
	      [OldSize, Size, Sizes]),
    [atom_to_frameid(ID),
     ununsynch(TagFlags, Size),
     render(TagFlags, Flags),
     <<TextEncoding>>,
     Text
    ];
render(_TagFlags, F) when is_record(F, id3v2_frame_flags)->
    <<
     0:1,
     (bool_to_int(F#id3v2_frame_flags.tag_alter_discard)):1,
     (bool_to_int(F#id3v2_frame_flags.file_alter_discard)):1,
     (bool_to_int(F#id3v2_frame_flags.read_only)):1,

     0:1, 0:1, 0:1, 0:1,

     0:1,
     (bool_to_int(F#id3v2_frame_flags.grouping_identity)):1,
     0:1,
     0:1,

     (bool_to_int(F#id3v2_frame_flags.compressed)):1,
     (bool_to_int(F#id3v2_frame_flags.encrypted)):1,
     (bool_to_int(F#id3v2_frame_flags.unsynch)):1,
     (bool_to_int(F#id3v2_frame_flags.data_length_indicator)):1
     >>;
render(TagFlags, List) when is_list(List) ->
    [ render(TagFlags, Item) || Item <- List ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unit Test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


test_item(FileName) ->
    {P, Rest} = parse_file(FileName),
    io:format("~s:~n  ~P~n", [FileName, P,length(P#id3v2_tag.frames)+50]),
    msleep(1000),
    R = render(P),
    msleep(1000),
    file:write_file("id3parse-test.mp3", [R,Rest]),
    P.


test([], Acc) ->
    lists:reverse(Acc);
test([FileName|Tail], Acc) ->
    test(Tail, [test_item(FileName)|Acc]).


test(List) when is_list(List) ->
    test(List, []).


test() ->
    test([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Frame ID Name Database
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


frameid_name(FrameID) when is_atom(FrameID) ->
    frameid_name(atom_to_list(FrameID));
frameid_name(FrameID) when is_binary(FrameID) ->
    frameid_name(binary_to_list(FrameID));
frameid_name(FrameID) when is_list(FrameID) ->
    case ets:info(?TAG_TABLE) of
	undefined -> start();
	_ -> ok
    end,
    case ets:lookup(?TAG_TABLE, FrameID) of
	[]                 -> FrameID;
	[{FrameID, Value}] -> Value
    end.


start() ->
    ets:new(?TAG_TABLE, [set, protected, named_table]),
    ets:insert(?TAG_TABLE, {"TIT1", "Content Group Description"}),
    ets:insert(?TAG_TABLE, {"TIT2", "Title/songname/content description"}),
    ets:insert(?TAG_TABLE, {"TIT3", "Subtitle/Description refinement"}),
    ok.

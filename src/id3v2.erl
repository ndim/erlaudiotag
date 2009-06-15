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
%% FIXME: Avoid encoding arbitrary strings from user data as atoms.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(id3v2).


-export([test/1, test/0]).
-export([parse_data/1, parse_file/1]).
-export([render/1]).


-import(ndim_bpu, [dump_bytes/2, msleep/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Include record definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-include("id3v2.hrl").
-include_lib("kernel/include/file.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define(TAG_TABLE, id3_frame_ids).


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


unsynch_int(true,
	    <<
	     0:1, S3:7,
	     0:1, S2:7,
	     0:1, S1:7,
	     0:1, S0:7
	     >>) ->
    (((((S3 bsl 7) + S2) bsl 7) + S1) bsl 7) + S0;
unsynch_int(false, <<S:32/integer>>) ->
    S.


unsynch(#id3v2_tag_flags{unsynch=Bool}, Bin) ->
    unsynch_int(Bool, Bin);
unsynch(#id3v2_frame_flags{unsynch=Bool}, Bin) ->
    unsynch_int(Bool, Bin).


shiftop(Val) ->
    {Val band 16#7f, Val bsr 7}.


ununsynch_int(true, Val) ->
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
     >>;
ununsynch_int(false, Val) ->
    <<Val:32/integer>>.


ununsynch(#id3v2_tag_flags{unsynch=Bool}, Size) when is_integer(Size) ->
    ununsynch_int(Bool, Size);
ununsynch(#id3v2_frame_flags{unsynch=Bool}, Size) when is_integer(Size) ->
    ununsynch_int(Bool, Size).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse ID3v2 tags from file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


parse_file(FileName) ->
    io:format("parse_file(\"~s\")~n", [FileName]),
    {ok, File} = file:open(FileName, [read, raw, binary]),
    {ok, HeadData} = file:read(File, 10),
    case HeadData of
	<<"ID3", VerMajor, _VerMinor, _Flags:1/binary, Size:4/binary>>
	when (3 =< VerMajor)
	     ->
	    %% FIXME: WHY true, why not TagFlags?
	    RealSize = unsynch_int(true, Size),
	    %% +10 for the potential footer
	    {ok, Data} = file:read(File, RealSize+10),
	    file:close(File),
	    parse_tag(HeadData, Data);
	_ ->
	    file:close(File),
	    {error, no_id3v2_tag_at_sof, HeadData}
    end.


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


parse_data(<<
	   "ID3",
	   VerMajor, VerMinor,
	   Flags:1/binary,
	   Size:4/binary,
	   Rest/binary
	   >>) ->
    parse_tag(<<"ID3", VerMajor, VerMinor, Flags:1/binary, Size:4/binary>>,
	      Rest);
parse_data(Bin) when is_binary(Bin) ->
    <<Hdr:10/binary, _/binary>> = Bin,
    {error, no_id3v2_tag_hdr, Hdr}.


parse_tag(<<"ID3", VerMajor, VerMinor, Flags:1/binary, Size:4/binary>>,
	  Rest)
  when is_binary(Rest)
       ->
    TagFlags = parse_tag_flags(Flags),
    RealSize = unsynch_int(true, Size), % FIXME: WHY true, why not TagFlags?
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
    <<
     TagData:RealSize/binary,
     AfterTag/binary
     >> = Rest,
    {ExtHdr, Rest1}  = parse_extended_header(TagFlags, TagData),
    {Frames, Rest2}  = parse_frame(TagFlags, Rest1, []),
    {PadSize, Rest3} = skip_padding(Rest2, 0),
    {Footer, <<>>}  = parse_footer(TagFlags, Rest3),
    {ok,
     #id3v2_tag{version=#id3v2_tag_version{major=VerMajor, minor=VerMinor},
		flags=TagFlags,
		size=RealSize,
		ext_hdr=ExtHdr,
		frames=Frames,
		padding=#id3v2_padding{size=PadSize},
		footer=Footer
	       },
     AfterTag}.


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
	    <<0, _/binary>> = Rest, Acc) ->
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
    dump_bytes(Rest, "Following data"),

    FrameFlags = parse_frame_flags(FrameFlagsRaw),

    <<
     FrameData:FrameSize/binary,
     NewRest/binary
     >> = Rest,

    dump_bytes(FrameData, "Frame Data"),

    io:format("parse_frame id=\"~s\" size=~w=~w flags=~w=~w~n",
	      [binary_to_list(FrameID),
	       FrameSizeUnsynch, FrameSize,
	       FrameFlagsRaw, FrameFlags]),

    parse_frame_int(TagFlags, FrameFlags,
		    FrameID, FrameFlags, FrameData,
		    NewRest, Acc).


text_content_int(_TextEncoding, <<>>, Acc) ->
    {lists:reverse(Acc), <<>>};

%% TextEncoding = 0, -> latin1
text_content_int(0, <<0/integer, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
text_content_int(0 = TextEncoding, <<Char/integer, Rest/binary>>, Acc) ->
    text_content_int(TextEncoding, Rest, [Char|Acc]);

%% TextEncoding = 1, -> utf16 with BOM
text_content_int(1, <<0,0, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
text_content_int(1, <<255, 254, Rest/binary>>, Acc) ->
    text_content_int(little, Rest, Acc);
text_content_int(1, <<254, 255, Rest/binary>>, Acc) ->
    text_content_int(2, Rest, Acc); % big endian

%% TextEncoding = 2, -> utf16be without BOM
text_content_int(2, <<0,0, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
text_content_int(2 = TextEncoding, <<Char/big-utf16, Rest/binary>>, Acc) ->
    text_content_int(TextEncoding, Rest, [Char|Acc]);

%% TextEncoding = little -> utf16le without BOM
text_content_int(little, <<0,0, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
text_content_int(little = TextEncoding, <<Char/little-utf16, Rest/binary>>, Acc) ->
    text_content_int(TextEncoding, Rest, [Char|Acc]);

%% TextEncoding = 2, -> utf8
text_content_int(3, <<0/utf8, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
text_content_int(3 = TextEncoding, <<Char/utf8, Rest/binary>>, Acc) ->
    text_content_int(TextEncoding, Rest, [Char|Acc]).

text_content_int(TextEncoding, Text) when is_binary(Text) ->
    text_content_int(TextEncoding, Text, []).


text_content(_TextEncoding, <<>>, Acc) ->
    lists:reverse(Acc);
text_content(TextEncoding, Text, Acc) ->
    {String, NewText} = text_content_int(TextEncoding, Text),
    text_content(TextEncoding, NewText, [String|Acc]).


text_content(TextEncoding, Text) when is_binary(Text) ->
    case false of
	true ->
	    io:format("text_content(~s,~n"
		      "             ~P)~n",
		      [case TextEncoding of
			   0 -> "latin1";
			   1 -> "utf16-with-BOM";
			   2 -> "utf16be";
			   3 -> "utf8"
		       end,
		       Text, 100]);
	false -> ok
    end,
    text_content(TextEncoding, Text, []).


frameid_atom(FrameID) when is_binary(FrameID) ->
    frameid_atom(binary_to_list(FrameID));
frameid_atom(FrameID) when is_list(FrameID) ->
    list_to_atom(FrameID).


atom_to_frameid(FrameID) when is_atom(FrameID) ->
    list_to_binary(atom_to_list(FrameID)).


parse_frame_int(#id3v2_tag_flags{unsynch=_Unsync, footer=_HasFooter} = TagFlags,
		FrameFlags,
		<<"TXXX">> = FrameID, FrameFlags, Payload, Rest, Acc) ->
    parse_frame(TagFlags,
		Rest,
		[#id3v2_frame
		 {id=frameid_atom(FrameID),
		  name=frameid_name(FrameID),
		  flags=FrameFlags,
		  orig_payload=Payload,
		  payload=#id3v2_generic_frame{}}|Acc]);
parse_frame_int(#id3v2_tag_flags{unsynch=_Unsync, footer=_HasFooter} = TagFlags,
		FrameFlags,
		<<"T",T1:8/integer,T2:8/integer,T3:8/integer>> = FrameID,
		FrameFlags,
		<<TextEncoding:8/integer, Text/binary>> = Payload,
		Rest, Acc)
  when ($0 =< T1), (T1 =< $Z),
       ($0 =< T2), (T2 =< $Z),
       ($0 =< T3), (T3 =< $Z)
       ->
    io:format("  Text tag: ~s~n"
	      "  Encoding: ~w~n",
	      [binary_to_list(FrameID),
	       TextEncoding]),
    io:format("   Content: ~w ~p~n", [size(Text), Text]),
    Content = text_content(TextEncoding, Text),
    io:format("   Content: ~p~n", [Content]),
    [Characters] = Content,
    parse_frame(TagFlags,
		Rest,
		[#id3v2_frame{id=frameid_atom(FrameID),
			      name=frameid_name(FrameID),
			      flags=FrameFlags,
			      orig_payload=Payload,
			      payload=#id3v2_text_frame{
				text_encoding=TextEncoding,
				text=Characters}}|Acc]);
parse_frame_int(#id3v2_tag_flags{unsynch=_Unsync, footer=_HasFooter} = TagFlags,
		FrameFlags,
		<<"USLT">> = FrameID,
		FrameFlags,
		<<TextEncoding:8/integer,
		 Language:3/binary,
		 Text/binary>> = Payload,
		Rest, Acc) ->
    io:format("  Text tag: ~s~n"
	      "  Encoding: ~w~n"
	      "  Language: ~w~n"
	      ,
	      [binary_to_list(FrameID),
	       TextEncoding, Language]),
    io:format("   Content: ~w ~P~n", [size(Text), Text, 100]),
    Moo = [ContentDescriptor, LyricsText] = text_content(TextEncoding, Text),
    io:format("   Content: ~P~n", [Moo, 100]),
    io:format("   Lyrics: ~s~n", [<< <<C/utf8>> || C<-LyricsText>>]),
    parse_frame(TagFlags,
		Rest,
		[#id3v2_frame{id=frameid_atom(FrameID),
			      name=frameid_name(FrameID),
			      flags=FrameFlags,
			      orig_payload=Payload,
			      payload=#id3v2_uslt_frame{
				text_encoding=TextEncoding,
				language=Language,
				content_descriptor=ContentDescriptor,
				lyrics_text=LyricsText}}|Acc]);
parse_frame_int(#id3v2_tag_flags{unsynch=_Unsync, footer=_HasFooter} = TagFlags,
		FrameFlags,
		<<"APIC">> = FrameID,
		FrameFlags,
		<<
		 TextEncoding:8/integer,
		 Stuff/binary
		 >> = Payload,
		Rest, Acc) ->
    dump_bytes(Stuff, "Stuff"),
    {MimeType, Data1} = text_content_int(TextEncoding, Stuff),
    dump_bytes(Data1, "Data1"),
    <<PictureType:8/integer, Data2/binary>> = Data1,
    dump_bytes(Data2, "Data2"),
    {Description, ImgData} = text_content_int(TextEncoding, Data2),
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
			      orig_payload=Payload,
			      payload=#id3v2_apic_frame{
				text_encoding=TextEncoding,
				description=Description,
				mime_type=MimeType,
				pic_type=PictureType,
				img_data=ImgData}}|Acc]);
parse_frame_int(#id3v2_tag_flags{unsynch=_Unsync, footer=_HasFooter} = TagFlags,
		FrameFlags,
		<<T0:8/integer,T1:8/integer,T2:8/integer,T3:8/integer>>=FrameID,
		FrameFlags, Payload, Rest, Acc)
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
			      orig_payload=Payload,
			      payload=#id3v2_generic_frame{}}|Acc]).


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


render_text_int(0, List) ->
    unicode:characters_to_binary(List, unicode, latin1);
render_text_int(1, "") ->
    <<>>;
render_text_int(1, List) ->
    Encoding = utf16,
    [unicode:encoding_to_bom(Encoding),
     unicode:characters_to_binary(List, unicode, Encoding)];
render_text_int(2, List) ->
    unicode:characters_to_binary(List, unicode, {utf16, big});
render_text_int(3, List) ->
    unicode:characters_to_binary(List, unicode, utf8).


render_text(_TextEncoding, [], Acc) ->
    lists:reverse(Acc);
%%render_text(TextEncoding, [Head], Acc) ->
%%    render_text(TextEncoding, [],
%%		[render_text_int(TextEncoding, Head) | Acc]);
render_text(0 = TextEncoding, [Head|Tail], Acc) ->
    render_text(TextEncoding, Tail,
		[<<0>>, render_text_int(TextEncoding, Head) | Acc]);
render_text(1 = TextEncoding, [Head|Tail], Acc) ->
    render_text(TextEncoding, Tail,
		[<<0,0>>, render_text_int(TextEncoding, Head) | Acc]);
render_text(2 = TextEncoding, [Head|Tail], Acc) ->
    render_text(TextEncoding, Tail,
		[<<0,0>>, render_text_int(TextEncoding, Head) | Acc]);
render_text(3 = TextEncoding, [Head|Tail], Acc) ->
    render_text(TextEncoding, Tail,
		[<<0>>, render_text_int(TextEncoding, Head) | Acc]).


render_text(TextEncoding, Text) ->
    render_text(TextEncoding, Text, []).


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
     ununsynch_int(true, Size),  % FIXME: WHY true, why not TagFlags?
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
render(TagFlags, #id3v2_frame{id=FrameID, flags=FrameFlags,
			      orig_payload=OrigPayload,
			      payload=Payload}) ->
    UseOriginal = true,
    %% Note that the case explictly does not handle the two errorneous cases.
    RP = render_payload
	   (case UseOriginal of
		false when is_binary(OrigPayload),
			   is_record(Payload, id3v2_generic_frame) ->
		    OrigPayload;
		false when is_binary(OrigPayload) ->
		    Payload;
		false when not is_record(Payload, id3v2_generic_frame) ->
		    Payload;
		true when is_binary(OrigPayload) ->
		    OrigPayload;
		true when not is_record(Payload, id3v2_generic_frame) ->
		    Payload
	    end),
    RPL = iolist_size(RP),
    [atom_to_frameid(FrameID),
     ununsynch(TagFlags, RPL),
     render(TagFlags, FrameFlags),
     RP];
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


render_payload(Binary) when is_binary(Binary) ->
    Binary;
render_payload(
  #id3v2_apic_frame{text_encoding=TextEncoding,
		    description=Description,
		    mime_type=MimeType,
		    pic_type=PicType,
		    img_data=ImgData
		   }) ->
    [TextEncoding,
     MimeType,0,
     PicType,
     Description,0,
     ImgData];
render_payload(
  #id3v2_text_frame{text_encoding=TextEncoding,
		    text=Content}) ->
    [<<TextEncoding>>,
     render_text_int(TextEncoding, Content)
    ];
render_payload(
  #id3v2_uslt_frame{text_encoding=TextEncoding,
		    language=Language,
		    content_descriptor=ContentDescriptor,
		    lyrics_text=LyricsText}) ->
    Content = [ContentDescriptor, LyricsText],
    Text = render_text(TextEncoding, Content),
    [<<TextEncoding:8/integer>>,
     Language,
     Text
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unit Test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


test_filename(FileName, DoRenderTest) ->
    io:format("test_filename(~p)~n", [FileName]),
    case parse_file(FileName) of
	{ok, P, Rest} ->
	    io:format("~s:~n  ~P~n",
		      [FileName, P,length(P#id3v2_tag.frames)+50]),
	    msleep(1000),
	    case DoRenderTest of
		true ->
		    R = render(P),
		    file:write_file("id3parse-test.mp3", [R,Rest]);
		false ->
		    ok
	    end,
	    P;
	{error, Error, Data} ->
	    io:format("~s:~n  ~s~n  ~P~n",
		      [FileName, Error, Data, 50]),
	    {error, Error}
    end.


test_item(FileName) ->
    {ok, FileInfo} = file:read_file_info(FileName),
    case FileInfo#file_info.type of
	regular ->
	    test_filename(FileName, true);
	directory ->
	    {ok, Filenames} = file:list_dir(FileName),
	    [test_item(filename:join(FileName,Item)) || Item <- Filenames]
    end.


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

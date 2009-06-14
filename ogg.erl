-module(ogg).


-export([test/0, test/1]).
-export([parse_file/1]).


parse_file(FileName) ->
    {ok, File} = file:open(FileName, [read, raw, binary]),
    {ok, HeadData} = file:read(File, 1024),
    case HeadData of
	<<
	 "OggS", % capture_pattern
	 _/binary>> ->
	    file:close(File),
	    parse_data(HeadData);
	_ ->
	    file:close(File),
	    {error, not_an_ogg_file_header, HeadData}
    end.


parse_data(Data) ->
    parse_ogg(Data).


parse_ogg(<<
	    "OggS", % capture_pattern
	    0, % stream_structure_version,

	    %% header_type_flag
	    0:5, % reserved flags
	    EOS:1, % end of stream
	    BOS:1, % beginning of stream
	    Cont:1, % continued packet

	    %% absolute granule position
	    AbsGranulePos:8/binary,

	    %% stream serial number
	    StreamSerial:32,

	    %% page sequence number
	    PageSeqNo:32,

	    %% page checksum
	    PageCRC:32,

	    PageSegments:8,
	    SegmentTable:PageSegments/binary,

	    Rest/binary
	    >>) ->
    io:format("parse_ogg~n"
	      "  EOS, BOS, Cont: ~w ~w ~w~n"
	      "  AbsGranulePos:  ~w~n"
	      "  StreamSerial:   ~.16#~n"
	      "  PageSeqNo:      ~w~n"
	      "  PageCRC:        ~.16#~n"
	      "  PageSegments:   ~w~n"
	      "  SegmentTable:   ~w~n",
	      [
	       EOS, BOS, Cont,
	       AbsGranulePos,
	       StreamSerial,
	       PageSeqNo,
	       PageCRC,
	       PageSegments,
	       SegmentTable
	      ]),
    id3v2:dump_bytes(Rest, "Rest"),
    parse_vorbis(Rest).


%% Vorbis bitstream Common header
%% http://xiph.org/vorbis/doc/Vorbis_I_spec.html#x1-610004.2.1
parse_vorbis(<<1, "vorbis", Rest/binary>>) ->
    parse_vorbis_header(identification, Rest);
parse_vorbis(<<3, "vorbis", Rest/binary>>) ->
    parse_vorbis_header(comment, Rest);
parse_vorbis(<<5, "vorbis", Rest/binary>>) ->
    parse_vorbis_header(setup, Rest).


parse_vorbis_header(identification,
		    <<
		     VorbisVersion:32/little-unsigned-integer,
		     AudioChannels:8/little-unsigned-integer,
		     AudioSampleRate:32/little-unsigned-integer,
		     BitrateMaximum:32/little-signed-integer,
		     BitrateNominal:32/little-signed-integer,
		     BitrateMinimum:32/little-signed-integer,
		     Blocksize0:4/little-unsigned-integer,
		     Blocksize1:4/little-unsigned-integer,
		     Padding:7/little-integer,
		     FramingFlag:1/little-unsigned-integer,
		     Rest/binary>>)
  when (VorbisVersion =:= 0),
       (AudioChannels > 0),
       (AudioSampleRate > 0),
       (FramingFlag =/= 0)
     ->
    %% Identification Header
    %% http://xiph.org/vorbis/doc/Vorbis_I_spec.html#x1-620004.2.2
    io:format("Vorbis Identification Header:~n"
	      "  VorbisVersion:           ~.16# ~w~n"
	      "  AudioChannels:           ~w~n"
	      "  Bitrate (Max, Nom, Min): ~w ~w ~w~n"
	      "  Blocksize:               ~w ~w~n"
	      "  FramingFlag:             ~w~n"
	      "  Padding:                 ~.16# ~w~n"
	      ,
	      [
	       VorbisVersion, VorbisVersion,
	       AudioChannels,
	       BitrateMaximum,
	       BitrateNominal,
	       BitrateMinimum,
	       Blocksize0, Blocksize1,
	       FramingFlag,
	       Padding, Padding
	      ]),
    id3v2:dump_bytes(Rest, "Rest"),
    parse_ogg(Rest);
parse_vorbis_header(comment,
		    <<
		     VendorLength:32/little-unsigned-integer,
		     VendorString:VendorLength/binary,
		     UserCommentListLength:32/little-unsigned-integer,
		     Rest/binary
		     >>) ->
    %% Comment Header
    %% http://xiph.org/vorbis/doc/Vorbis_I_spec.html#x1-610004.2.3
    %% http://xiph.org/vorbis/doc/Vorbis_I_spec.html#x1-810005.2.1
    io:format("Vorbis Comment Header:~n"
	      "  Vendor:                ~w \"~s\"~n"
	      "  UserCommentListLength: ~w~n",
	      [
	       VendorLength, VendorString,
	       UserCommentListLength
	      ]),
    id3v2:dump_bytes(Rest, "UserComment Data"),
    {Comments, Rest1} = parse_user_comments(UserCommentListLength, Rest, []),
    id3v2:dump_bytes(Rest1, "Rest1"),
    <<
     0:7, % padding
     1:1, % framing bit
     Rest2/binary
     >> = Rest1,
    [ io:format("  Comment: ~s~n", [C]) || C <- Comments ],
    id3v2:dump_bytes(Rest2, "Rest2"),
    parse_vorbis(Rest2);
parse_vorbis_header(setup, <<Rest/binary>>) ->
    %% Setup Header
    %% http://xiph.org/vorbis/doc/Vorbis_I_spec.html#x1-610004.2.4
    {stuff, Rest}.


parse_user_comments(0, Rest, Acc) ->
    {lists:reverse(Acc), Rest};
parse_user_comments(UserCommentsListLength,
		    <<
		     Length:32/little-unsigned-integer,
		     Comment:Length/binary,
		     Rest/binary
		     >>,
		    Acc)
  when is_integer(UserCommentsListLength),
       UserCommentsListLength > 0,
       is_list(Acc)
  ->
    parse_user_comments(UserCommentsListLength-1,
			Rest,
			[Comment|Acc]).


test_item(FileName) ->
    io:format("~s: ~n", [FileName]),
    P = parse_file(FileName),
    io:format("  ~P~n", [P,100]),
    P.


test([], Acc) ->
    lists:reverse(Acc);
test([Head|Tail], Acc) ->
    test(Tail, [test_item(Head)|Acc]).


test(List) when is_list(List) ->
    test(List, []).


test() ->
    test([]).


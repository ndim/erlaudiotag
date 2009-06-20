%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ogg.erl - Parse OGG Vorbis files for meta information
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


-module(ogg).


-export([test/0, test/1]).
-export([parse_file/1]).


-import(ndim_bpu, [dump_bytes/2]).


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
    dump_bytes(Rest, "Rest"),
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
    dump_bytes(Rest, "Rest"),
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
    dump_bytes(Rest, "UserComment Data"),
    {Comments, Rest1} = parse_user_comments(UserCommentListLength, Rest, []),
    dump_bytes(Rest1, "Rest1"),
    <<
     0:7, % padding
     1:1, % framing bit
     Rest2/binary
     >> = Rest1,
    [ begin
	  {K,V} = split_comment(C),
	  io:format("  Comment: ~-13s \"~s\"~n", [K,V])
      end || C <- Comments ],
    dump_bytes(Rest2, "Rest2"),
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


split_comment(<<"=", Rest/binary>>, AccKey) ->
    {list_to_binary(lists:reverse(AccKey)), Rest};
split_comment(<<Char, Rest/binary>>, AccKey) ->
    split_comment(Rest, [Char|AccKey]).

split_comment(Binary) when is_binary(Binary) ->
    split_comment(Binary, []).


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


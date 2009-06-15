-module(unicode).


-export([encoding_to_bom/1]).
-export([characters_to_binary/3]).


encoding_to_bom(utf16) ->
    encoding_to_bom({utf16,little});

encoding_to_bom({utf16,little}) ->
    <<255,254>>;

encoding_to_bom({utf16,big}) ->
    <<254,255>>.


characters_to_binary(List, unicode, utf16) ->
    characters_to_binary(List, unicode, {utf16,little});

characters_to_binary(List, unicode, {utf16,little}) ->
    << <<C/utf16-little>> || C<-List >>;

characters_to_binary(List, unicode, {utf16,big}) ->
    << <<C/utf16-big>> || C<-List >>;

characters_to_binary(List, unicode, utf8) ->
    << <<C/utf8>> || C<-List >>;

characters_to_binary(List, unicode, latin1) ->
    << <<C>> || C<-List >>.

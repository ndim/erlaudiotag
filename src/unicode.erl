%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% unicode.erl - compatibility module
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
%% This unicode module aims to provide the most urgently needed functions
%% from Erlang R13's unicode module for R12 for a specific application.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

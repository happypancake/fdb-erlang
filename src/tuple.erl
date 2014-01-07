-module(tuple).
-export([pack/1, unpack/1]).

-define(TERMINATOR,0).
-define(LIST, 1).
-define(TUPLE, 2).
-define(BINARY, 3).
-define(FLOAT, 4).
-define(INTEGER, 40).
-define(ESCAPE, 256).

pack([]) ->
  <<?TERMINATOR>>; 
pack(Data) when is_list(Data) ->
  <<(encode_range(Data, <<?LIST>>))/binary, ?TERMINATOR>>;
pack(Data) when is_tuple(Data) ->
  <<(encode_range(tuple_to_list(Data), <<?TUPLE>>))/binary, ?TERMINATOR>>;
pack(Data) when is_binary(Data) ->
  <<?BINARY, (escape_terminator(Data))/binary, ?TERMINATOR>>;
%% use == 0, so it works for both ints and floats
pack(N) when N == 0 -> <<?INTEGER, ?TERMINATOR>>;
pack(N) when is_integer(N) -> number(N);
pack(N) when is_float(N) -> <<?FLOAT, N/float, 0>>.
  
unpack(<<?INTEGER, ?TERMINATOR>>) -> 0;
unpack(<<?FLOAT, Val/float, ?TERMINATOR>>) -> Val;
unpack(<<?BINARY, Remainder/binary>>) ->
  ?TERMINATOR = binary:last(Remainder),
  Part = binary:part(Remainder, 0, byte_size(Remainder)-1),
  unescape_terminator(Part).
  
  
encode_range([], Result) ->
  escape_terminator(Result);
encode_range([H|T], Result) ->
  encode_range(T,<<Result/binary,(pack(H))/binary>>).

escape_terminator(Data) ->
  binary:replace(Data, <<?TERMINATOR>>, <<?TERMINATOR, ?ESCAPE>>,[global]).

unescape_terminator(Data) ->
  binary:replace(Data, <<?TERMINATOR, ?ESCAPE>>, <<?TERMINATOR>>, [global]).


number(N) ->
  ByteCount = numbersize(N),
  <<(?INTEGER+ByteCount),(number(N, ByteCount))/binary,?TERMINATOR>>.

number(N, Len) when Len<0 -> number((1 bsl ((-Len)*8))+N, -Len);
number(N, Len) -> <<N:(Len*8)>>.

%% use pattern matching as opposed to a list, so it's faster I assume
numbersize(N) when N < 0 -> -numbersize(-N);
numbersize(N) when N < 16#100 -> 1;
numbersize(N) when N < 16#10000 -> 2;
numbersize(N) when N < 16#1000000 -> 3;
numbersize(N) when N < 16#100000000 -> 4;
numbersize(N) when N < 16#10000000000 -> 5;
numbersize(N) when N < 16#1000000000000 -> 6;
numbersize(N) when N < 16#100000000000000 -> 7;
numbersize(N) when N < 16#10000000000000000 -> 8;
numbersize(N) when N < 16#1000000000000000000 -> 9.

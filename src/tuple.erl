-module(tuple).
-export([pack/1, unpack/1]).

-define(TERMINATOR,0).
-define(LIST, 1).
-define(TUPLE, 2).
-define(BINARY, 3).
-define(NUMBER, 40).
-define(ESCAPE, 256).

pack([]) ->
  <<?TERMINATOR>>; 
pack(Data) when is_list(Data) ->
  <<(encode_range(Data, <<?LIST>>))/binary, ?TERMINATOR>>;
pack(Data) when is_tuple(Data) ->
  <<(encode_range(tuple_to_list(Data), <<?TUPLE>>))/binary, ?TERMINATOR>>;
pack(Data) when is_binary(Data) ->
  <<?BINARY, (escape_terminator(Data))/binary, ?TERMINATOR>>;
pack(N) when is_number(N)-> <<(?NUMBER+numbersize(N)),N/float>>.
  
unpack(_) ->
  undefined.  
  
encode_range([], Result) ->
  escape_terminator(Result);
encode_range([H|T], Result) ->
  encode_range(T,<<Result/binary,(pack(H))/binary>>).

escape_terminator(Data) ->
  binary:replace(Data, <<?TERMINATOR>>, <<?TERMINATOR, ?ESCAPE>>,[global]).

numbersize(N) when N < 0 -> -numbersize(-N);
numbersize(N) when N == 0 -> 0;
numbersize(N) when N < 16#100 -> 1;
numbersize(N) when N < 16#10000 -> 2;
numbersize(N) when N < 16#1000000 -> 3;
numbersize(N) when N < 16#100000000 -> 4;
numbersize(N) when N < 16#10000000000 -> 5;
numbersize(N) when N < 16#1000000000000 -> 6;
numbersize(N) when N < 16#100000000000000 -> 7;
numbersize(N) when N < 16#10000000000000000 -> 8;
numbersize(N) when N < 16#1000000000000000000 -> 9.

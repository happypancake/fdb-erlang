-module(tuple).
-export([pack/1, unpack/1]).

-define(LIST, 1).
-define(TUPLE, 2).
-define(BINARY, 3).
-define(FLOAT, 4).
-define(INTEGER, 40).
-define(ESCAPE, 254).
-define(TERMINATOR,255).

pack({do_not_pack,X}) -> X;
pack([]) ->
  <<?TERMINATOR>>; 
pack(Data) when is_list(Data) ->
  <<?LIST, (encode_range(Data,<<>>))/binary, ?TERMINATOR>>;
pack(Data) when is_tuple(Data) ->
  <<?TUPLE, (encode_range(tuple_to_list(Data), <<>>))/binary, ?TERMINATOR>>;
pack(Data) when is_binary(Data) ->
  <<?BINARY, (escape_terminator(Data))/binary, ?TERMINATOR>>;
%% use == 0, so it works for both ints and floats
pack(0) -> <<?INTEGER, ?TERMINATOR>>;
pack(N) when is_integer(N) -> number(N);
pack(N) when is_float(N) -> <<?FLOAT, N/float, ?TERMINATOR>>.

unpack(<<?FLOAT, Val/float, ?TERMINATOR>>) -> Val;
unpack(<<?BINARY, Remainder/binary>>) ->
  Part = verify_and_extract(Remainder),
  unescape_terminator(Part);
unpack(<<?LIST, Remainder/binary>>) ->
  Part = verify_and_extract(Remainder),
  Unescaped = unescape_terminator(Part),
  Splitted = split_on_terminator(Unescaped),
  lists:map(fun unpack/1, Splitted);
unpack(<<?TUPLE, Remainder/binary>>) ->
  Part = verify_and_extract(Remainder),
  Unescaped = unescape_terminator(Part),
  Splitted = split_on_terminator(Unescaped),
  List = lists:map(fun unpack/1, Splitted),
  list_to_tuple(List);
%% Erlang pattern matching should be faster
unpack(<<?INTEGER, ?TERMINATOR>>) -> 0;
unpack(<<(?INTEGER+1):8, V:08,?TERMINATOR>>) -> V;
unpack(<<(?INTEGER+2):8, V:16,?TERMINATOR>>) -> V; 
unpack(<<(?INTEGER+3):8, V:24,?TERMINATOR>>) -> V;
unpack(<<(?INTEGER+4):8, V:32,?TERMINATOR>>) -> V;
unpack(<<(?INTEGER+5):8, V:40,?TERMINATOR>>) -> V;
unpack(<<(?INTEGER+6):8, V:48,?TERMINATOR>>) -> V;
unpack(<<(?INTEGER+7):8, V:52,?TERMINATOR>>) -> V;
unpack(<<(?INTEGER+8):8, V:64,?TERMINATOR>>) -> V;
unpack(<<(?INTEGER+9):8, V:72,?TERMINATOR>>) -> V;
unpack(<<(?INTEGER-1):8, V:08,?TERMINATOR>>) -> 16#100 - V;
unpack(<<(?INTEGER-2):8, V:16,?TERMINATOR>>) -> 16#10000 - V; 
unpack(<<(?INTEGER-3):8, V:24,?TERMINATOR>>) -> 16#1000000 - V;
unpack(<<(?INTEGER-4):8, V:32,?TERMINATOR>>) -> 16#100000000 - V;
unpack(<<(?INTEGER-5):8, V:40,?TERMINATOR>>) -> 16#10000000000 - V;
unpack(<<(?INTEGER-6):8, V:48,?TERMINATOR>>) -> 16#1000000000000 - V;
unpack(<<(?INTEGER-7):8, V:52,?TERMINATOR>>) -> 16#100000000000000 - V;
unpack(<<(?INTEGER-8):8, V:64,?TERMINATOR>>) -> 16#10000000000000000 - V;
unpack(<<(?INTEGER-9):8, V:72,?TERMINATOR>>) -> 16#1000000000000000000 - V.

verify_and_extract(Data) ->
  ?TERMINATOR = binary:last(Data),
  binary:part(Data, 0, byte_size(Data)-1).

split_on_terminator(Data) ->
  Splitted = binary:split(Data,<<?TERMINATOR>>,[global,trim]),
  join_escaped_splits(Splitted,[]).

join_escaped_splits([],Result) ->
  lists:reverse(Result);
join_escaped_splits([<<?ESCAPE,Data/binary>>|Todo], [H|T]) ->
  join_escaped_splits(Todo,[<<H/binary,?TERMINATOR,Data/binary>>|T]);
join_escaped_splits([H|T],R) ->
  join_escaped_splits(T,[<<H/binary,?TERMINATOR>> | R]).
  
encode_range([], Result) ->
  escape_terminator(Result);
encode_range([H|T], R) ->
  encode_range(T,<<R/binary,(pack(H))/binary>>).

escape_terminator(Data) ->
  binary:replace(Data, <<?TERMINATOR>>, <<?ESCAPE,?TERMINATOR>>,[global]).

unescape_terminator(Data) ->
  binary:replace(Data, <<?ESCAPE, ?TERMINATOR>>, <<?TERMINATOR>>, [global]).


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

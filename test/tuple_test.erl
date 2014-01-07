-module(tuple_test).

-include_lib("eunit/include/eunit.hrl").

-define(assertPacked(Expected,Data), ?assertEqual(Expected,tuple:pack(Data))).
-define(assertPackUnpack(Data), ?assertEqual(Data,tuple:unpack(tuple:pack(Data)))).

pack_test() ->
  ?assertPacked(<<3, "Hello world", 0>>, <<"Hello world">>),
  ?assertPacked(<<3, "Hello", 0, 256, "world", 0>>, <<"Hello",0,"world">>),
  ?assertPacked(<<1,3,"Hello",0,256,0>>, [<<"Hello">>]),
  ?assertPacked(<<1,3,"A",0,256,3,"B",0,256,0>>, [<<"A">>,<<"B">>]),
  ?assertPacked(<<40,0>>, 0),
  ?assertPacked(<<40,0>>, 0.0),
  ?assertPacked(<<41,16,0>>, 16),
  ?assertPacked(<<39,240,0>>, -16),
  ?assertPacked(<<43,70000:24,0>>, 70000),
  ?assertPacked(<<37,(16#1000000 - 70000):24,0>>, -70000).

pack_order_test() ->
  ?assert(tuple:pack({<<"A">>,16.3})>tuple:pack({<<"A">>,16.2})),
  ?assert(tuple:pack({["AZERTY"]})<tuple:pack({["QWERTY"]})).

unpack_test() ->
  ?assertPackUnpack(<<"Hello world">>),
  ?assertPackUnpack(<<"Hello",0,"world">>),
  ?assertPackUnpack(0),
  ?assertPackUnpack(1.1).



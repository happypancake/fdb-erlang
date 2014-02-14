-module(tuple_test).

-include_lib("eunit/include/eunit.hrl").

-define(assertPacked(Expected,Data), ?assertEqual(Expected,tuple:pack(Data))).
-define(assertPackUnpack(Data), ?assertEqual(Data,tuple:unpack(tuple:pack(Data)))).

-define(ESC,254).
-define(TERM,255).

pack_test() ->
  ?assertPacked(<<3,"Hello world", ?TERM>>, <<"Hello world">>),
  ?assertPacked(<<3,"Hello",?ESC,?TERM,"world",?TERM>>,<<"Hello",?TERM,"world">>),
  ?assertPacked(<<1,3,"Hello",?ESC,?TERM,?TERM>>, [<<"Hello">>]),
  ?assertPacked(<<1,3,"A",?ESC,?TERM,3,"B",?ESC,?TERM,?TERM>>, [<<"A">>,<<"B">>]),
  ?assertPacked(<<40,?TERM>>, 0),
  ?assertPacked(<<4,0.0/float,?TERM>>, 0.0),
  ?assertPacked(<<41,16,?TERM>>, 16),
  ?assertPacked(<<39,240,?TERM>>, -16),
  ?assertPacked(<<43,70000:24,?TERM>>, 70000),
  ?assertPacked(<<37,(16#1000000 - 70000):24,?TERM>>, -70000).

pack_order_test() ->
  ?assert(tuple:pack({<<"A">>,16.3})>tuple:pack({<<"A">>,16.2})),
  ?assert(tuple:pack({["AZERTY"]})<tuple:pack({["QWERTY"]})).

unpack_test() ->
  ?assertPackUnpack(<<"Hello world">>),
  ?assertPackUnpack(<<"Hello",0,"world">>),
  ?assertPackUnpack(0),
  ?assertPackUnpack(1.1),
  ?assertPackUnpack({<<"Hello">>, 1.1, 5}).



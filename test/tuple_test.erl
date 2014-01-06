-module(tuple_test).

-include_lib("eunit/include/eunit.hrl").

pack_test() ->
  ?assertEqual(<<3,"Hello world",0>>, tuple:pack(<<"Hello world">>)),
  ?assertEqual(<<3,"Hello",0,256,"world",0>>, tuple:pack(<<"Hello",0,"world">>)),
  ?assertEqual(<<0>>, tuple:pack([])),
  ?assertEqual(<<1,3,"Hello",0,256,0>>, tuple:pack([<<"Hello">>])),
  ?assertEqual(<<1,3,"A",0,256,3,"B",0,256,0>>, tuple:pack([<<"A">>,<<"B">>])),
  ?assertEqual(<<40,0>>, tuple:pack(0)),
  ?assertEqual(<<40,0>>, tuple:pack(0.0)),
  ?assertEqual(<<41,16,0>>, tuple:pack(16)),
  ?assertEqual(<<39,240,0>>, tuple:pack(-16)),
  ?assertEqual(<<43,70000:24,0>>, tuple:pack(70000)),
  ?assertEqual(<<37,(16#1000000 - 70000):24,0>>, tuple:pack(-70000)),
  ?assert(tuple:pack({<<"A">>,16.3})>tuple:pack({<<"A">>,16.2})),
  ?assert(tuple:pack({["AZERTY"]})<tuple:pack({["QWERTY"]})).



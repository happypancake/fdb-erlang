-module(tuple_test).

-include_lib("eunit/include/eunit.hrl").

pack_test() ->
  ?assertEqual(<<3,"Hello world",0>>, tuple:pack(<<"Hello world">>)),
  ?assertEqual(<<3,"Hello",0,256,"world",0>>, tuple:pack(<<"Hello",0,"world">>)),
  ?assertEqual(<<0>>, tuple:pack([])),
  ?assertEqual(<<1,3,"Hello",0,256,0>>, tuple:pack([<<"Hello">>])),
  ?assertEqual(<<1,3,"A",0,256,3,"B",0,256,0>>, tuple:pack([<<"A">>,<<"B">>])),
  ?assertEqual(<<40,0/float>>, tuple:pack(0)),
  ?assertEqual(<<41,16/float>>, tuple:pack(16)),
  ?assertEqual(<<43,70000.5/float>>, tuple:pack(70000.5)),
  ?assertEqual(<<37,-70000.8/float>>, tuple:pack(-70000.8)),
  ?assert(tuple:pack({<<"A">>,16.3})>tuple:pack({<<"A">>,16.2})),
  ?assert(tuple:pack({["AZERTY"]})<tuple:pack({["QWERTY"]})).



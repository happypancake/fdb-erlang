-module(directory_test).


-include_lib("eunit/include/eunit.hrl").

-define(SOLIB,"../priv/fdb_nif").

basic_test_todo()->
  DB = fdb:init_and_open(?SOLIB),
  DirA = directory:open(DB,<<"a">>),
  DirB = directory:open(DB,<<"b">>),
  directory:set(DirA, <<"key">>, <<"A">>),
  directory:set(DirB, <<"key">>, <<"B">>),
  ?assertEqual(<<"A">>, directory:get(DirA, <<"key">>)),
  ?assertEqual(<<"B">>, directory:get(DirB, <<"key">>)),
  ?assertEqual([{<<"key">>,<<"B">>}], 
              directory:get_range(DirB,<<"a">>,<<"z">>)),
  ?assertEqual(<<"_a">>,directory:get_name(DirA)),
  directory:clear(DirA, <<"key">>),
  directory:clear(DirB, <<"key">>).
 

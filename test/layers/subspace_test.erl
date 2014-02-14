-module(subspace_test).

-include_lib("eunit/include/eunit.hrl").

-define(SOLIB,"../priv/fdb_nif").

basic_test()->
  DB = fdb:init_and_open(?SOLIB),
  SpaceA = subspace:open(DB,<<"_a">>),
  SpaceB = subspace:open(DB,<<"_b">>),
  subspace:set(SpaceA, <<"key">>, <<"A">>),
  subspace:set(SpaceB, <<"key">>, <<"B">>),
  ?assertEqual(<<"A">>, subspace:get(SpaceA, <<"key">>)),
  ?assertEqual(<<"B">>, subspace:get(SpaceB, <<"key">>)),
  ?assertEqual([{<<"key">>,<<"B">>}], 
              subspace:get_range(SpaceB,<<"a">>,<<"z">>)),
  ?assertEqual(<<"_a">>,subspace:get_name(SpaceA)),
  subspace:clear(SpaceA, <<"key">>),
  subspace:clear(SpaceB, <<"key">>).
  

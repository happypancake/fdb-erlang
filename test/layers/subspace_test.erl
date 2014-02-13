-module(subspace_test).

-include_lib("eunit/include/eunit.hrl").

-define(SOLIB,"../priv/fdb_nif").

basic_test()->
  DB = fdb:init_and_open(?SOLIB),
  SpaceA = subspace:get_from_db(DB,<<"__subspace__a">>),
  SpaceB = subspace:get_from_db(DB,<<"__subspace__b">>),
  subspace:set(SpaceA, <<"somekey">>, <<"A">>),
  subspace:set(SpaceB, <<"somekey">>, <<"B">>),
  ?assertEqual(<<"A">>, subspace:get(SpaceA, <<"somekey">>)),
  ?assertEqual(<<"B">>, subspace:get(SpaceB, <<"somekey">>)),
  ?assertEqual([{<<"somekey">>,<<"B">>}], 
              subspace:get_range(SpaceB,<<"a">>,<<"z">>)),
  subspace:clear(SpaceA, <<"somekey">>),
  subspace:clear(SpaceB, <<"somekey">>).
  

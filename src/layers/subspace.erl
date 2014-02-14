-module(subspace).
-export([get/2, get/3, get_range/3, set/3]).
-export([clear/2, clear_range/3]).
-export([transact/2]).

-export([get_from_db/2]).
-export([get_name/1]).

-behaviour(gen_fdb).

-include("../../include/fdb.hrl").

get({SS,Handle}, K, D) -> fdb:get(Handle,ss_key(K,SS),D).

get({SS,Handle}, Select = #select{}) -> 
  Key = ss_key(Select,SS),
  io:format("Key: ~p~n",[Key]),
  fdb:maybe_do([
   fun() -> fdb:get(Handle, Key) end,
   fun(L) -> [{K, V}|| {{_,K},V} <- L] end
  ]);
get({SS,Handle}, K) -> fdb:get(Handle,ss_key(K,SS)).

get_range(H,Begin,End) -> get(H, #select{gte = Begin, lt = End}).

set({SS,H},K, V) -> fdb:set(H,ss_key(K,SS),V).

clear({SS,H},K) -> fdb:clear(H,ss_key(K,SS)).

clear_range({SS,H},Begin,End) -> fdb:clear_range(H,ss_key(Begin,SS),ss_key(End,SS)).

transact({_,DB}, DoStuff) -> fdb:transact(DB, DoStuff).

get_from_db(DB,SS) -> {SS,DB}.
get_name({SS,_DB}) -> SS.

ss_key(Select = #select{},SS) -> lt(gt(Select,SS),SS);
ss_key(V, SS) -> {SS,V}.

lt(Select = #select{lt=LT,lte=LTE},SS) ->
  case {LT,LTE} of
    {nil,nil} -> Select#select{lt=tuple:pack({SS,{do_not_pack,<<255>>}})};
    {LT,nil} -> Select#select{lt=tuple:pack({SS,LT})};
    {_,LTE} -> Select#select{lte=tuple:pack({SS,LTE})}
  end.

gt(Select = #select{gt=GT,gte=GTE},SS) ->
  case {GT,GTE} of
    {nil,nil} -> Select#select{gt=tuple:pack({SS,{do_not_pack,<<0>>}})};
    {GT,nil} -> Select#select{gt=tuple:pack({SS,GT})};
    {_,GTE} -> Select#select{gte=tuple:pack({SS,GTE})}
  end.


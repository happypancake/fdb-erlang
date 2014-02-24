-module(subspace).
-export([get/2, get/3, get_range/3, set/3]).
-export([clear/2, clear_range/3]).
-export([transact/2]).

-export([open/2]).
-export([get_name/1,get_handle/1]).
-export([all/1]).

-behaviour(gen_fdb).

-include("../../include/fdb.hrl").

get({ss,SS,Handle}, K, D) -> fdb:get(Handle,ss_key(K,SS),D).

get({ss,SS,Handle}, Select = #select{}) -> 
  Key = ss_key(Select,SS),
  io:format("Key: ~p~n",[Key]),
  fdb:maybe_do([
   fun() -> fdb:get(Handle, Key) end,
   fun(L) -> [{K, V}|| {{_,K},V} <- L] end
  ]);
get({ss,SS,Handle}, K) -> fdb:get(Handle,ss_key(K,SS)).

get_range(H,Begin,End) -> get(H, #select{gte = Begin, lt = End}).

set({ss,SS,H},K, V) -> fdb:set(H,ss_key(K,SS),V).

clear({ss,SS,H},K) -> fdb:clear(H,ss_key(K,SS)).

clear_range({ss,SS,H},Begin,End) -> fdb:clear_range(H,ss_key(Begin,SS),ss_key(End,SS)).

transact({ss,_,DB}, DoStuff) -> fdb:transact(DB, DoStuff).

open(DB={db,_},SS) -> {ss,SS,DB};
open(Tx={tx,_},SS) -> {ss,SS,Tx}.

get_name({ss,SS,_}) -> SS.
get_handle({ss,_,H}) -> H.

all(SS={ss,_,_}) -> 
  get_range(SS,{do_not_pack,<<0>>},{do_not_pack,<<255>>}).

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


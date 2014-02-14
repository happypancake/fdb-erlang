-module(directory).

-export([get/2, get/3, get_range/3, set/3]).
-export([clear/2, clear_range/3]).
-export([transact/2]).

-export([open/1,open/2,open/3]).
-export([get_subspace/1,get_path/1]).
-export([list/1]).

-behavior(gen_fdb).

-include("../../include/fdb.hrl").

get(D = {dir, _, _}, Select = #select{}) -> subspace:get(get_subspace(D),Select);
get(D = {dir, _, _}, Key) -> subspace:get(get_subspace(D),Key).

get(D = {dir, _, _}, Key, Default) -> subspace:get(get_subspace(D),Key, Default).

set(D = {dir, _, _}, Key, Value) -> subspace:set(get_subspace(D),Key, Value).

get_range(D = {dir, _, _}, Begin, End) -> subspace:get_range(get_subspace(D),Begin,End).

clear(D = {dir, _, _}, Key) -> subspace:clear(get_subspace(D),Key).
  
clear_range(D = {dir, _, _}, Begin, End) -> subspace:clear_range(get_subspace(D),Begin,End).

transact(D = {dir, _, _}, DoStuff) -> subspace:transact(get_subspace(D),DoStuff).

-define(DEFAULT_SUBSPACE,<<"default_directories">>).

open(Handle={T,_},SS_Name, Path) when T == db orelse T == tx ->
  open(subspace:open(Handle,SS_Name),Path).

open(SS={ss,_SS,_Handle}) ->
  open(SS,[]);
open(Handle={T,_}) when T == db orelse T == tx ->
  open(Handle, ?DEFAULT_SUBSPACE).

open(Handle={T,_},SS_Name) when T == db orelse T == tx ->
  open(Handle,SS_Name,[]);
open({dir,SS,Path},RelativePath) ->
  {dir, SS, navigate(Path,RelativePath)};
open(SS={ss,_SS,_Handle},Path) ->
  {dir,SS,Path}.

list(D = {dir, _, _}) -> subspace:list(get_subspace(D)).

navigate(Path,[]) -> Path;
navigate(_Path,[root|Xs])->
  navigate([],Xs);
navigate(Path=[_|_],[parent|Xs]) -> 
  NewPath = lists:sublist(Path,length(Path)-1),
  navigate(NewPath,Xs);
navigate(Path,[X|Xs]) -> navigate(Path ++ [X],Xs).

get_path({dir,_,Path}) -> Path.

get_subspace({dir,SS,Path}) ->
  H = subspace:get_handle(SS),
  N = subspace:get_name(SS),
  subspace:open(H, {N, Path}).

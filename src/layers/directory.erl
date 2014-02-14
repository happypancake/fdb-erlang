-module(directory).

%-export([get/2, get/3, get_range/3, set/3]).
%-export([clear/2, clear_range/3]).
%-export([transact/2]).

-export([open/1,open/2,open/3]).

%-behavior(gen_fdb).

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

navigate(Path,[]) -> Path;
navigate(_Path,[root|Xs])->
  navigate([],Xs);
navigate(Path=[_|_],[parent|Xs]) -> 
  NewPath = lists:sublist(Path,length(Path)-1),
  navigate(NewPath,Xs);
navigate(Path,[X|Xs]) -> navigate(Path ++ [X],Xs).


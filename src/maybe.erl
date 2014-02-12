-module(maybe).

-export([do/1,do/2]).

do([Fn|N]) -> do(Fn(),N).

do(Value, [])          -> Value;
do({error, Error}, _N) -> {error, Error};
do(Value, [Fn|N]) ->
  case erlang:fun_info(Fn, arity) of
    {arity, 0} -> 
         Result = Fn(),
         case Result of
           {error, _} -> Result;
           {ok, NewValue} -> do(NewValue, N);
           NewValue -> do(NewValue, N)
         end;
    {arity, 1} -> 
         Result = case Value of
           {ok, V} -> Fn(V);
           Other -> Fn(Other)
         end,
         do(Result,N);
    Other -> throw({wrong_arity,[Other,{funcs_remaining, length(N)}]})
  end.

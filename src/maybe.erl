-module(maybe).

-export([do/1,do/2]).

do([Fn|N]) -> do(Fn(),N).

do({ok, Value}, [])    -> Value;
do(Value, [])          -> Value;
do({error, Error}, _N) -> {error, Error};
do({ok, Value}, N)     -> do(Value,N);
do(Value, [Fn|N])      -> do(Fn(Value),N).
  

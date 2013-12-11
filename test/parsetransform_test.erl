-module(parsetransform_test).

-include_lib("eunit/include/eunit.hrl").

simplest_test() ->
  parse_transform(


simple_xform([{attribute, AttrLine, wrap_with, AttrArgs},
              {function, FuncLine, FuncName, Arity, Args} | Remainder],Done) ->
   simple_xform(Remainder, [

simple_xform([Element|Remainder], Done) ->
   simple_xform(Remainder, [Element|Done]).
  
simple_xform({Type, Line, name, Pars}) ->

-wrap_with(inc_first_arg).
inc(arg) ->
  arg+1.

inc_first_arg([M, F, A]) ->
  [First|Others] = call(M, F, A)
  [First+1|Others].

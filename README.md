# fdb-erlang

Erlang bindings for [FoundationDB](https://foundationdb.com/).

This is still very early alpha.

## Todo

- [ ] A non-VM-blocking `fdb_nif:fdb_future_block_until_ready` using `enif_send` and `fdb_future_set_callback`. More inspiration [here](http://www.erlang-factory.com/upload/presentations/370/paul-davis-zero-to-emonk.pdf).
- [x] Conversion from Erlang terms to keys using [the FDB tuple layer](https://foundationdb.com/documentation/api-python.html#api-python-tuple-layer).
- [ ] Map most-used functionality. (A lot of function calls return `not_implemented`.)

## Prerequisites

Developed and tested on ubuntu only (for now).
It requires you to have Erlang and [the FoundationDB client libraries installed](https://foundationdb.com/documentation/api-general.html#installing-client-binaries).

## Compilation and testing

```bash
git clone git@github.com:HappyPancake/fdb-erlang.git
cd fdb-erlang
chmod u+x rebar
./rebar get-deps compile eunit -v
```

## Example usage

This is the simplest possible usage:
```erlang
tojans@ubuntu:/tmp/fdb-erlang$ erl -pa ebin
Erlang R16B02 (erts-5.10.3) [source-5d89ddd] [64-bit] [async-threads:10] [kernel-poll:false]

Eshell V5.10.3  (abort with ^G)
1> fdb:init().
ok
2> fdb:api_version(100).
0
3> DB = fdb:open().
{db,<<>>}
4> fdb:get(DB,<<"Hello">>).
not_found
5> fdb:set(DB,<<"Hello">>,<<"World">>).
ok
6> fdb:get(DB,<<"Hello">>).            
<<"World">>
7> fdb:get(DB,<<"Hello2">>,some_default_value).
some_default_value
8> fdb:clear(DB,<<"Hello">>).
ok
9> fdb:get(DB,<<"Hello">>).
not_found
10>
```

If you want to have a transaction, you need to invoke code like this:
```erlang
10> fdb:transact(DB, fun(Tx) ->                         
10>   fdb:set(Tx,<<"Hello">>,<<"World">>),            
10>   fdb:set(Tx,<<"xyz">>,<<"abc">>),                
10>   [fdb:get(Tx,<<"xyz">>)|fdb:get(Tx,<<"Hello">>)]   
10> end).
[<<"abc">>|<<"World">>]
11>
```
It will invoke the lambda multiple times until the whole action succeeds or it seems unreasonable to try any longer.
If the code in the lambda crashes, the transaction will be rolled back.

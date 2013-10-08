# fdb-erlang

Erlang bindings for [FoundationDB](https://foundationdb.com/).

This is currently still very early alpha.

## Prerequisites

Currently this is only tested on ubuntu.
This requires you to have Erlang and [the FoundationDB client libraries installed](https://foundationdb.com/documentation/api-general.html#installing-client-binaries).

## Compilation and testing

```
git clone git@github.com:HappyPancake/fdb-erlang.git
cd fdb-erlang
chmod u+x rebar
./rebar get-deps compile eunit -v
```

This is the output
```
tojans@ubuntu:/tmp$ git clone git@github.com:HappyPancake/fdb-erlang.git
Cloning into 'fdb-erlang'...
remote: Counting objects: 154, done.
remote: Compressing objects: 100% (122/122), done.
remote: Total 154 (delta 66), reused 99 (delta 17)
Receiving objects: 100% (154/154), 170.51 KiB, done.
Resolving deltas: 100% (66/66), done.
tojans@ubuntu:/tmp$ cd fdb-erlang/
tojans@ubuntu:/tmp/fdb-erlang$ chmod u+x rebar
tojans@ubuntu:/tmp/fdb-erlang$ ./rebar get-deps compile eunit -v
WARN:  Expected /tmp/fdb-erlang/deps/gen_driver to be an app dir (containing ebin/*.app), but no .app found.
==> fdb-erlang (get-deps)
WARN:  Expected /tmp/fdb-erlang/deps/gen_driver to be an app dir (containing ebin/*.app), but no .app found.
Pulling gen_driver from {git,"git://github.com/HappyPancake/generic-linked-in-driver.git",
                             "master"}
Cloning into 'gen_driver'...
==> gen_driver (get-deps)
==> gen_driver (compile)
Compiled src/gen_driver_test.erl
Compiled src/gen_driver.erl
Compiling c_src/gen_driver.c
Compiling c_src/gen_driver_test.c
==> fdb-erlang (compile)
Compiled src/fdb.erl
Compiling c_src/fdb_drv.c
==> gen_driver (eunit)
Compiled src/gen_driver_test.erl
Compiled src/gen_driver.erl
Compiled test/basic_test.erl
======================== EUnit ========================
module 'basic_test'
  basic_test: setup_and_teardown_test...[0.003 s] ok
  basic_test: basic_test...[0.509 s] ok
  [done in 0.517 s]
module 'gen_driver'
module 'gen_driver_test'
=======================================================
  All 2 tests passed.
==> fdb-erlang (eunit)
Compiled src/fdb.erl
Compiled test/fdb_test.erl
======================== EUnit ========================
module 'fdb'
module 'fdb_test'
  fdb_test: api_version_test...[0.001 s] ok
  fdb_test: setup_network_test...[0.001 s] ok
  fdb_test: run_network_test...[0.001 s] ok
  fdb_test: cluster_test...[0.001 s] ok
  fdb_test: database_test...[0.001 s] ok
  fdb_test: transaction_test...[0.003 s] ok
  fdb_test: basic_storage_test...[0.013 s] ok
  [done in 0.037 s]
=======================================================
  All 7 tests passed.
```

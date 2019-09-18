# Ordered Key-Value Stores Libraries

Like it is explained in the SRFI document, there are various database
libraries that can be used to implement the SRFI 167 library.

## FoundationDB

FoundationDB (FDB) is a distributed database that supports ACID
transactions.  It has a client library written in C that support both
synchronous and asynchronous code and several independent client
libraries, in particular nodejs and erlang.

SRFI 167 is based on FDB as it is the common denominator of Ordered
Key-Value stores.  In particular, SRFI 167 doesn't include 'previous'
and 'next' procedures that would allow one to navigate the key space
because FDB doesn't provide such a thing.

Homepage: https://foundationdb.org
License: Apache 2

## Sqlite LSM4

Homepage: https://sqlite.org/src4/doc/trunk/www/lsmusr.wiki#performance_tuning
License: None

## LevelDB

LevelDB is widely available ordered key value store.  It doesn't
support transactions.  It has C bindings. If you need transactions
with liberal license you most likely want to use RocksDB see below.

Like SRFI 167 and FDB, it exposes a single key namespace.

Chicken: https://wiki.call-cc.org/eggref/4/leveldb
Gerbil: https://cons.io/guide/intro.html#key-value-stores

Homepage: https://github.com/google/leveldb
License: BSD

## RocksDB

RocksDB is fork of LevelDB that, among other things, supports
transactions.

Homepage: https://rocksdb.org/
License: Apache 2 or GPLv2

## LMDB

LMDB is another widely available ordered key-value store that is used
in OpenLDAP.  You can find various benchmarks on its website,
including this one: http://www.lmdb.tech/bench/ondisk/

Gerbil: https://cons.io/guide/intro.html#key-value-stores

Homepage: https://lmdb.tech
License: OpenLDAP Public License

## WiredTiger

Unlike SRFI-167, WiredTiger exposes multiple key-value spaces.  It has
a notion of cursor and allows one to navigate key space with `previous`
and `next`.  It is a superset of SRFI 167.

Guile: https://framagit.org/a-guile-mind/guile-wiredtiger/#guile-wiredtiger
Chez: https://git.sr.ht/~amz3/azul/tree/master/src/cffi/wiredtiger.scm

Homepage: http://www.wiredtiger.com/
License: GPLv2

## Kyoto Cabinet

Kyoto Cabinet is primarly a dbm replacement, but it can be a suitable backend
for SRFI 167 thanks to its B+ Tree support.

Homepage: https://fallabs.com/kyotocabinet/
License: GPLv3

## Oracle BerkeleyDB

Homepage: https://www.oracle.com/database/technologies/related/berkeleydb.html
License: AGPLv3

## TiKV

Another distributed ordered key-value store.

Homepage: https://tikv.org/
License: Apache 2

## sophia

Homepage: http://sophia.systems/
License: BSD

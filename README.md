# ENCOINS Relay App

Encoins Relay server and console client applications. Instead of running following commands through cabal, you can use [this script](https://github.com/encryptedcoins/encoins-relay/blob/main/build.sh) to build all executables, and run them directly from [encoins-tools](https://github.com/encryptedcoins/encoins-tools) (you still need cabal for this).

# Server usage:

1. Run encoins-relay server:</br>
```console
$ cabal run encoins-relay-server -- run
```

2. Mint and send encoins beacon token:</br>
```console
$ cabal run encoins-relay-server -- setup
```

3. Run verifier server:</br>
```console
$ cabal run encoins-relay-verifier
```

# Client usage:

1. Run client in which it will generate and send a request to selected endpoint with an average *interval* seconds:</br>
```console
$ cabal run encoins-relay-client -- [ping | funds | newTx | serverTx | submitTx | status] --auto interval
```
&emsp;&emsp;For example:
```console
$ cabal run encoins-relay-client -- serverTx --auto 30
```

2. Run client in manual mode in which it will burn specified encoins *tokens* or mint tokens equals to specified amount of *ada*:</br>
```console
$ cabal run encoins-relay-client -- [ping | funds | newTx | serverTx | submitTx | status] --manual [mada|btoken],[mada|btoken], ...
```
&emsp;&emsp;For example:
```console
$ cabal run encoins-relay-client -- serverTx --manual m30,m4,b0x05e229b5959b43c4cf5358e1687d68d253fb08aa96068ab47f19f2be7207d9ec
```
Note that the maximum number of tokens in one request is 5.

## ServerTx and NewTx endpoints:

1. `ServerTx` and `NewTx` endpoints can also work in ledger mode as follows:
```console
$ cabal run encoins-relay-client -- serverTx LedgerMode --manual m1,b0xad5b57acbaf171803b94696847c8fa0114bd5df16bcd71fcbcb15b4d3bad761d
```

2.  These endpoints can also be used to make transactions that produce utxos:
```console
$ cabal run encoins-relay-client -- --manual "addr_test1vrnptddmxuwzqqnx8f40ljfggdsfc3hmzmzvnsjtt9wzhdclkhhn5,Value {coin = \"2000000\", multiasset = Just (MultiAsset (fromList [(\"05338963283a4c49b8007aa37b7c2f36ab882f108eb524846fda7949\",fromList [(\"\",\"1\")])]))}"
```

## Status endpoint:

1. Get the maximum amount of ada that can be taken from a single utxo bound to a ledger address:
```console
$ cabal run encoins-relay-client -- status --manual ada
```

2. Get all ledger utxos containing 6 or fewer tokens (including ada) and at least one encoins token:
```console
$ cabal run encoins-relay-client -- status --manual encoins
```
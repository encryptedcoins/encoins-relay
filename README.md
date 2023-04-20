# ENCOINS Relay App

Encoins Relay server and console client applications.

# Server usage:

1. Run server:</br>
```console
$ cabal run encoins-relay-server -- run
```

2. Mint and send encoins beacon token:</br>
```console
$ cabal run encoins-relay-server -- setup
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
$ cabal run encoins-relay-client -- newTx --manual m30,m4,b0x05e229b5959b43c4cf5358e1687d68d253fb08aa96068ab47f19f2be7207d9ec
```
Note that the maximum number of tokens in one request is 5.

`ServerTx` and `NewTx` endpoints can also work in ledger mode as follows:
```console
$ cabal run encoins-relay-client -- serverTx LedgerMode --manual m1,b0xad5b57acbaf171803b94696847c8fa0114bd5df16bcd71fcbcb15b4d3bad761d
```
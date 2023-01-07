# ENCOINS-relay

Encoins-relay server and console client applications.

# Server usage:

1. Run server:</br>
```console
$ cabal run encoinsRelayServer -- run
```

2. Mint and send encoins beacon token:</br>
```console
$ cabal run encoinsRelayServer -- setup
```

# Client usage:

1. Run client in automatic mode in which it will send up to *maximum* encoins tokens at an average *interval* seconds:</br>
```console
$ cabal run encoinsRelayClient -- --auto -i interval -m maximum
```
&emsp;&emsp;For example:
```console
$ cabal run encoinsRelayClient -- --auto -i 30 -m 5
```

2. Run client in manual mode in which it will burn specified encoins *tokens* or mint tokens equals to specified amount of *ada*:</br>
```console
$ cabal run encoinsRelayClient -- --manual --[mint ada|burn token] --[mint ada|burn token] ...
```
&emsp;&emsp;For example:
```console
$ cabal run encoinsRelayClient -- --manual --mint 25 --burn 7a4a75b76c66759849b462dd443fc902c64b94aa6cb19f50607eb58f463b5c67
```

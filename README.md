# ENCOINS-relay

Encoins-relay server and client applications.

# Server usage:

1. Run server:</br>
cabal run encoinsRelayServer -- run

2. Mint and send encoins beacon token:</br>
cabal run encoinsRelayServer -- setup

# Client usage:

1. Run client in automatic mode in which it will send up to *maximum* encoins tokens at an average *interval*:</br>
cabal run encoinsRelayClient -- --auto -i *interval* -m *maximum*

2. Run client in manual mode in which it will burn specified encoins *tokens* or mint tokens equals to specified amount of *ada*:</br>
cabal run encoinsRelayClient -- --manual --[mint *ada*|burn *token*] --[mint *ada*|burn *token*] ...
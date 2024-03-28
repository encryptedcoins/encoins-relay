# ENCOINS Tools
A suite of scripts and config files to run the ENCOINS backend applications.

# Installation
In order to set up all the necessary applications for the ENCOINS relay, consult the installation guide [here](https://github.com/encryptedcoins/encoins-tools/blob/main/INSTALL.md). The guide has been tested on a clean Ubuntu 22.04.3 LTS with the minimal installation option.

# Run
* Make changes to the config files if necessary.
* [Add your wallet](https://github.com/encryptedcoins/encoins-tools#cardano-wallet)
* Launch the [run.sh](https://github.com/encryptedcoins/encoins-tools/blob/main/run.sh) script or use the guide [here](https://github.com/encryptedcoins/encoins-tools/blob/main/RUN.md).
* After executing ```encoins --run```, make sure that port 3000 is accessible from the outside. You can test port accessibility [here](https://www.yougetsignal.com/tools/open-ports/).

# Reward Distribution
* Launch all necessary services by executing the [run.sh](https://github.com/encryptedcoins/encoins-tools/blob/main/run.sh) script or by using the guide [here](https://github.com/encryptedcoins/encoins-tools/blob/main/RUN.md).
* Execute ```encoins --reward X``` to distribute X ada as rewards to your delegators.
* Wait until the `encoins` app exits.

How much ada you want to distribute and how much to keep for yourself is up to you!

**About debts**: Upon completion, you will see `debts.json` file in the `mainnet/apps/encoins` folder. It records ada debts to delegators that are too small to distribute. If you have such delegators, you will distribute a little bit less than X ada. Next time you run ```encoins --reward```, it will also try to pay the existing debts, so make sure you have enough ada to also cover those.

# Notes

## cardano-node

* Use `./getNetworkConfig.sh` to download the latest default config files for the cardano-node.
* When node is synchronized and running, use `./getProtocolParameters.sh` to download the current protocol parameters in JSON format.
* To start a cardano-node with the default parameters, go to "scripts" folder and run `./node.sh`.

## cardano-wallet

* Backend wallets are stored in the "wallets" folder. Change the `mnemonic_senstence` in the "wallet-example.json" file to the seed phrase of your backend wallet and save it as "wallet.json".
* To start a cardano-wallet app, go to "scripts" folder and run `./wallet.sh`.

IMPORTANT: it is strongly recommended not to store large amounts of crypto in such backend wallets.

## Data providers
To use an external data provider, you need the corresponding tokens.

* You can get a free Blockfrost token by registering at https://blockfrost.io/. Write your token in quotes in the "blockfrost.token" file inside "mainnet/apps/encoins" folder (see "blockfrost.token.example" there).
* You can get a free Maestro token by registering at https://gomaestro.org/. Write your token in quotes in the "maestro.token" file inside "mainnet/apps/encoins" folder (see "maestro.token.example" there).
* To use lightweight wallet-provider (no cardano-node required) set `wallet_provider` field in `config.json` to
```
"wallet_provider" : {"tag" : "Lightweight", "addresses" : ["addr1", "addr2", ...]}
```
where `addresses` - list with your wallet tracked addresses in bech32. Don't set too many addresses, or you will very quickly burn all your maestro/blockfrost credits. Just one address will be enough.
* To use `Maestro` chainindex provider set `chain_index_provider` field in `config.json` to `"Maestro"`.
* To use `Maestro` transaction provider set `tx_provider` field in `config.json` to `"Maestro"`.
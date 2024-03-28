# ENCOINS Relay installation guide

This guide is written for a clean Ubuntu 22.04.3 LTS (minimal installation option). To install the relay on another OS, you might need to make some adjustments. Steps 1-7 repeat the steps necessary for running a Cardano node.

1. Add ```.local/bin``` folder to path:

```bash
mkdir -p "$HOME/.local/bin/"
if [[ ! -d "$PATH" ]]; then
    export PATH="$HOME/.local/bin/:$PATH"
    echo "export PATH='$HOME/.local/bin/:$PATH'" >> ~/.bashrc
fi
```

2. Install the following packages:

```bash
sudo apt-get update -y && sudo apt-get upgrade -y
sudo apt-get install automake build-essential curl pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libtool autoconf libpq-dev -y
```

3. Create a temporary folder:

```bash
mkdir -p ~/cardano-src
export CARDANO_SRC_PATH=~/cardano-src
```

4. Install libsodium:

```bash
cd $CARDANO_SRC_PATH
git clone https://github.com/input-output-hk/libsodium
cd libsodium
git checkout dbb48cc
./autogen.sh
./configure
make
sudo make install
```

5. Export path variables:

```bash
if [[ ! -d $LD_LIBRARY_PATH ]]; then
    export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
    echo "export LD_LIBRARY_PATH='/usr/local/lib:$LD_LIBRARY_PATH'" >> ~/.bashrc
fi

if [[ ! -d "$PKG_CONFIG_PATH" ]]; then
    export PKG_CONFIG_PATH="usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
    echo "export PKG_CONFIG_PATH='/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH'" >> ~/.bashrc
fi
```

6. Install libsecp256k1:

```bash
cd $CARDANO_SRC_PATH
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git checkout ac83be33
./autogen.sh
./configure --enable-module-schnorrsig --enable-experimental
make
make check
sudo  make install
```

7. Install cardano-node and cardano-wallet:

```bash
cd $CARDANO_SRC_PATH
mkdir -p "cardano-wallet"
cd cardano-wallet
wget https://github.com/cardano-foundation/cardano-wallet/releases/download/v2023-07-18/cardano-wallet-v2023-07-18-linux64.tar.gz
tar -xvzf cardano-wallet-v2023-07-18-linux64.tar.gz
mv cardano-wallet-v2023-07-18-linux64/cardano-wallet "$HOME/.local/bin/"
wget https://github.com/encryptedcoins/encoins-relay/releases/download/v1.2.1/cardano-node
chmod +x cardano-node
mv cardano-node "$HOME/.local/bin/"
```

8. Install Kupo:

```bash
cd $CARDANO_SRC_PATH
mkdir -p "kupo"
cd kupo
wget https://github.com/CardanoSolutions/kupo/releases/download/v2.7/kupo-2.7.0-amd64-Linux.tar.gz
tar -xvzf kupo-2.7.0-amd64-Linux.tar.gz
chmod +x bin/kupo
mv bin/kupo "$HOME/.local/bin/"
```

9. Install encoins-relay:

```bash
cd $CARDANO_SRC_PATH
wget https://github.com/encryptedcoins/encoins-relay/releases/download/v1.2.1/encoins
chmod +x encoins
mv encoins "$HOME/.local/bin/"
```

10. Delete the temporary folder.

```bash
rm -f -r $CARDANO_SRC_PATH
```

11. Clone encoins-tools repository:
```bash
cd ~
git clone https://github.com/encryptedcoins/encoins-tools.git
export ENCOINS_TOOLS_PATH=~/encoins-tools
```

NOTE: you can update `encoins-tools` to the latest version by running `git pull`.

12. Start cardano-node and Kupo synchronization:
```bash
cd $ENCOINS_TOOLS_PATH
session="encoins-relay";
tmux new-session -d -s $session;
window=0;
tmux send-keys -t $session:$window "cd mainnet/scripts/" C-m ENTER;
tmux send-keys -t $session:$window "./node.sh" C-m ENTER;
tmux split-window -v;
tmux send-keys -t $session:$window "cd mainnet/scripts/" C-m ENTER;
tmux send-keys -t $session:$window "./kupo.sh" C-m ENTER;
tmux attach -t $session;
```

13. To properly shut down, use ```ctrl + c``` to close the individual apps. Then use ```ctrl + b + d``` to detach tmux. Finally, kill the tmux session with the following command:
```bash
tmux kill-session -t encoins-relay
```

14. Create a Cardano wallet for your relayer. You can use any browser wallet interface to do so. Go to ```mainnet/wallets``` folder and edit the mnemonic phrase in ```wallet-example.json``` file to your wallet mnemonics. Save as "wallet.json".
```bash 
cd $ENCOINS_TOOLS_PATH/mainnet/wallets
nano wallet.json
```

15. Add your relay's IP or URL to the config file. Go to ```mainnet/apps/encoins``` folder and edit ```relay-config.json``` file by changing the `delegation_ip` field to something like `http://1.2.3.4` (insert your relay's IP).

16. Deposit 20-30 tADA into your backend wallet address.

17. Make sure to open port 3000 on your relay machine. You will not be able to receive user requests otherwise.

Now you are ready to run the ENCOINS Relay!

Consult the [RUN.md](https://github.com/encryptedcoins/encoins-tools/blob/main/RUN.md) guide next.


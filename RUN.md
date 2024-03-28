# ENCOINS Relay run guide

Assuming you followed the installation guide, all the necessary backend apps should be installed and on PATH. Make an environment variable for the encoins-tools folder:
```bash
export ENCOINS_TOOLS_PATH=~/encoins-tools
```

IMPORTANT: Make sure to open port 3000 on your relayer machine. You will not be able to receive user requests otherwise.

## Running with a single bash command

1. Move to the encoins-tools folder and execute ```run.sh```.

2. Wait until cardano-wallet is fully [synchronized](https://github.com/encryptedcoins/encoins-tools/blob/main/RUN.md#When-is-cardano-wallet-fully-synchronized?).

3. Press enter.

## Running with tmux

1. In the terminal, move to the  ```testnet-preprod/scripts``` folder:

```code
cd $ENCOINS_TOOLS_PATH/testnet-preprod/scripts
```

2. Start a new tmux session:

```code
tmux new-session -d -s "encoins-relay"
tmux
```

3. Press ```ctrl + b + %``` to split window vertically and run cardano-node:

```tmux
./node.sh
```

4. Press ```ctrl + b + "``` to split window horizontally and run Kupo:

```tmux
./kupo.sh
```

5. Press ```ctrl + b + "``` to split window horizontally and run cardano-wallet:

```tmux
./wallet.sh
```

6. After running cardano-wallet split window again, move to the ```wallets``` directory and load your wallet:

```tmux
cd ../wallets
./load-wallet.sh
```

7. Close this pan with ```ctrl + b x``` when you see message like this:

```code
{"address_pool_gap":20,"assets":{"available":[],"total":[]},"balance":{"available":{"quantity":0,"unit":"lovelace"},"reward":{"quantity":0,"unit":"lovelace"} ...
```

8. Wait until cardano-wallet is fully [synchronized](https://github.com/encryptedcoins/encoins-tools/blob/main/RUN.md#When-is-cardano-wallet-fully-synchronized?).

9. Now select your first pan with ```ctrl + b q 0```, go to ```../apps/encoins``` directory and run encoins-relay:
```tmux
cd ../apps/encoins
encoins run
```

10. To properly shut down, use ```ctrl + c``` to close the individual apps. Then use ```ctrl + b + d``` to detach tmux. Finally, kill the tmux session with
```bash
tmux kill-session -t encoins-relay
```

## When is cardano-wallet fully synchronized?

When you see message like this:

```code
[cardano-wallet.wallet-engine:Notice:42] [current-time]: In sync! Applied n blocks...
```

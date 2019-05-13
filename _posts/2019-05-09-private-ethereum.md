---
layout: post
title: "Running a private Ethereum network in a Docker container"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- Blockchain
- Ethereum
- Docker
excerpt: How to setup a private Ethereum network in a Docker container.

---
Find the code at [Ethereum Private Network](https://github.com/arialdomartini/private-ethereum) on GitHub.

To define a network, we need:

- a unique genesis file
- the Go Ethereum client `geth`

Other peers can join the network if and only if they share the same genesis file.


## Genesis block
The genesis block defines a bunch of parameters each peer must agree on if they want to join the network.

The docker file stores the following file in the directory `/app/`:

```json
{
    "config": {
        "chainId": 99,
        "homesteadBlock": 0,
        "eip150Block": 0,
        "eip150Hash":"0x0000000000000000000000000000000000000000000000000000000000000000",
        "eip155Block": 0,
        "eip158Block": 0,
        "byzantiumBlock": 0,
        "constantinopleBlock": 0,
        "petersburgBlock":0
    },
    "nonce": "0x1200abc184050099",
    "timestamp": "0x00",
    "parentHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
    "extraData": "0x00",
    "gasLimit": "0x4c4b40",
    "difficulty": "0x0400",
    "mixhash": "0x0000000000000000000000000000000000000000000000000000000000000000",
    "coinbase": "0x0000000000000000000000000000000000000000",
    "alloc": {}
}
```

The notable fields are:

* `chainId`, the network id
* `difficulty`
* fields in the `config` section
* `alloc`

### Network id

The network id is a positive number. Avoid using 1, 2, 3 or 4, as they are reserved to existing public network:

* `1` main Ethereum network
* `2` Morden test network
* `3` Ropsten test network
* `4` Rinkeby test network

We set our private network id to `99` with

```json
"chainId": 99
```

### Difficulty

We set the difficulty to `1024` (`0x400` in hexadecimal) with:

```json
"difficulty": "0x0400"
```

This will enable fast mining. `geth` will eventualy adjust this value overtime. The default value, used in the main net, is `0x400000000`.

### Config
The `config` section defines the consensus protocols used, and at which block the network should start using each consensus protocol. It is in fact possible to switch from a consensus protocol to another, starting from a defined block, and have therefore a fork.

More than defining the beginning of a network, the genesis block defines the progression of protocols in a network.

### Pre-allocated ethereum
The genesis block can defined the balance of one or more accounts, specifying the number of wei to pre-allocate to each account:

```json
"alloc": {
        "0x1a7750aba798ea108ebc1873eab839910212855b": {
            "balance": "51000000000000000000"
        }
    }
```

We won't pre-allocate any ethereum, as we will be able to easily mine them, so our genesis block contains:


```json
"alloc": { }
```

## Start the network
To start the network, init it:

```bash
geth \
     --datadir /app/network99 \
     init \
     /app/genesis99.json
```

then connect to it with:

```bash
geth \
     --datadir /app/network99 \
     --networkid 99
```


## Operations

Let's read the difficulty:

```
eth.getBlock(0).difficulty
```



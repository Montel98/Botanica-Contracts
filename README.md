# Botanica-Smart-Contracts

This repository contains the current iteration of plutus scripts to be used in Botanica. Overviews and installation instructions are detailed below.

- [src/Botanica](src/Botanica) contains the actual Haskell source code.
- [app](app) contains the boilerplate scripts required to compile the Haskell source code to Plutus Bytecode.

## CounterNFT Plutus Script

This script creates an NFT by ensuring that a specific UTXO is spent with the transaction. This enforces 'non-fungibility' as a UTXO can only be spent once.

## CounterValidator Scripts

This setup consists of a minting policy script and a validator script. The validator script has a UTXO with a dummy NFT (minted by CounterNFT) and a datum attached to it that keeps track of the number of NFTs minted. In order to spend the UTXO at this address (to update the count), the following conditions must hold:

- The dummy NFT must be replaced at the validator script address
- The `AssetName` of the newly minted NFT must be equal to `currentCount + 1`
- The `AssetSymbol` of the newly minted NFT must be the hash (policyId) of the minting policy
- A datum must be attached to the replaced dummy NFT with the count incremented

The minting policy script allows for the NFT to be minted, it only checks that the validator script is provided as part of the transaction.

## NFTPolicy Script

This minting policy uses a redeemer to choose the following functionalities

- `Redeemer == 1` Mint a new NFT, using an NFT minted with 'CounterValidator'
- `Redeemer == 2` Burn and remint an NFT

1) This functionality is included specifically for Botanica, and can be omitted/removed for anyone wishing to use this script. It exists to circumvent the current script size limitations.

- The `AssetName` of the NFT to be minted must be suffixed with `A`
- There must be exactly 1 NFT from 'CounterValidator' present in 1 UTXO
- The NFT from 'CounterValidator' must be sent to the burn address
2) This functionality allows an NFT from this policy to be atomically burned and reminted, with the following conditions:

- The burned NFT must have been minted from the same script (same policyId)
- The reminted NFT must have the same asset name as the burned NFT, with the suffix flipped (from `A` to `B` OR `B` to `A`).

## Compiling the source code

With a Haskell environment set up, the script generating programs can be compiled by simply running `cabal update` and `cabal build`.

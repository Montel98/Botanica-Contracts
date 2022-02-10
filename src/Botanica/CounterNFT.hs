{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Botanica.CounterNFT
(
  mintScript,
  nftScriptShortBs
) where

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)


{- HLINT ignore "Avoid lambda" -}

{-# INLINABLE mkCounterNFTPolicy #-}
mkCounterNFTPolicy :: TokenName -> TxOutRef -> BuiltinData -> ScriptContext -> Bool
mkCounterNFTPolicy name utxo _ ctx = traceIfFalse "Wrong UTXO" hasUTxO &&
                                     traceIfFalse "Wrong amount" checkMintedAmount
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info

        checkMintedAmount :: Bool
        checkMintedAmount = case flattenValue (txInfoMint info) of
            [(s1, t1, a1)]               -> s1 == ownCurrencySymbol ctx && 
                                            t1 == name && 
                                            a1 == 1 {-&&
                                            s2 == ownCurrencySymbol ctx && 
                                            unTokenName t2 == "B" && 
                                            a2 == 1-}
            _                             -> False

nftPolicy :: TokenName -> TxOutRef -> Scripts.MintingPolicy
nftPolicy name utxo = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \name' utxo' -> Scripts.wrapMintingPolicy $ mkCounterNFTPolicy name' utxo' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode name
    `PlutusTx.applyCode`
    PlutusTx.liftCode utxo

nftPlutusScript :: TokenName -> TxOutRef -> Script
nftPlutusScript name utxo = unMintingPolicyScript $ nftPolicy name utxo

nftValidator :: TokenName -> TxOutRef -> Validator
nftValidator name utxo = Validator $ nftPlutusScript name utxo

nftScriptAsCbor :: TokenName -> TxOutRef -> LB.ByteString
nftScriptAsCbor name utxo = serialise $ nftValidator name utxo

nftScriptShortBs :: TokenName -> TxOutRef -> SBS.ShortByteString
nftScriptShortBs name utxo = SBS.toShort . LB.toStrict $ nftScriptAsCbor name utxo

mintScript :: TokenName -> TxOutRef -> PlutusScript PlutusScriptV1
mintScript name utxo = PlutusScriptSerialised . SBS.toShort . LB.toStrict $ nftScriptAsCbor name utxo
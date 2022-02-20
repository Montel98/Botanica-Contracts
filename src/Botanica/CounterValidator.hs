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

module Botanica.CounterValidator
(
  nftValidatorScript,
  nftValidatorScriptShortBs,
  nftPolicyScript,
  nftPolicyScriptShortBs,
  intToHexInner,
  NFTCounterParams(..)
  
) where

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import           Ledger.Ada               as Ada
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)

import qualified PlutusTx.AssocMap as Map

data NFTCounterParams = NFTCounterParams {
    counterSymbol :: !CurrencySymbol,
    nftSymbol :: !CurrencySymbol,
    appKeyHash :: !PubKeyHash
}

PlutusTx.makeLift ''NFTCounterParams

{- HLINT ignore "Avoid lambda" -}

{-# INLINABLE digitToHex #-}
digitToHex :: Integer -> BuiltinByteString
digitToHex digit = consByteString (48 + digit) emptyByteString


{-# INLINABLE intToHexInner #-}
intToHexInner :: Integer -> BuiltinByteString -> BuiltinByteString
intToHexInner count hexString
    | count < 10 && hexString == emptyByteString = digitToHex (count `remainder` 10)
    | count `quotient` 10 == 0 = hexString
    | otherwise = appendByteString (intToHexInner (count `quotient` 10) (digitToHex (count `quotient` 10))) (digitToHex (count `remainder` 10))

-- Creates the validator script for the NFT.
{-# INLINABLE mkValidator #-}
mkValidator :: NFTCounterParams -> Integer -> BuiltinData -> ScriptContext -> Bool
mkValidator cp d _ ctx = {-counterInOutput scriptOutput &&-}
                                  checkMintedAmount &&
                                  isNewCountValid d newCounter 
                                  && txSignedByApp
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        getCounterName :: TxOut -> BuiltinByteString
        getCounterName out = case flattenValue (txOutValue out) of
            [(s1, t1, _), (s2, t2, _)] | s2 == cSymbol && s1 == Ada.adaSymbol -> unTokenName t2
            _                                                                 -> traceError ""
            {-_                                                                 -> traceError "Counter misplaced"-}
        -- Get counter datum, if it exists
        newCounter :: Maybe Integer
        newCounter = do 
           dh                 <- txOutDatumHash $ scriptOutput
           Datum counterDatum <- findDatum dh info
           PlutusTx.fromBuiltinData counterDatum
           
        -- Get counter output
        scriptOutput :: TxOut
        scriptOutput = case getContinuingOutputs ctx of
            [out] -> out
            _     -> traceError ""
            {-_     -> traceError "Expected 1 output"-}
        
        -- Check that asset name is incremented and prefixed correctly
        isValidAssetName :: Integer -> TokenName -> Bool
        isValidAssetName oldCount name = assetNameCount == newCountHex
            where
                assetNameCount = unTokenName name
                newCountHex = appendByteString (getCounterName scriptOutput) $ intToHexInner (oldCount + 1) emptyByteString 
        
        -- Check if counter is incremented
        isNewCountValid :: Integer -> Maybe Integer -> Bool
        isNewCountValid old (Just new) = new == old + 1 &&
                                         new <= mintLimit
        isNewCountValid _ _ = False

        checkMintedAmount :: Bool
        checkMintedAmount = case flattenValue (txInfoMint info) of
            [(symbol', tokenName, amount)] -> symbol' == symbol && 
                                             isValidAssetName d tokenName && 
                                             amount == 1 
            _                              -> False

        txSignedByApp :: Bool
        txSignedByApp = txSignedBy info $ appKeyHash cp

        mintLimit :: Integer
        mintLimit = 1000

        cSymbol, symbol :: CurrencySymbol
        cSymbol = counterSymbol cp
        symbol = nftSymbol cp

{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: CurrencySymbol -> BuiltinData -> ScriptContext -> Bool
--mkNFTPolicy cSymbol r ctx = if r PlutusTx.Prelude.== 0 then counterSpent else burned
mkNFTPolicy cSymbol _ ctx = counterSpent
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        counterInInput :: TxOut -> Bool
        counterInInput out = case flattenValue (txOutValue out) of
            [(sym, _ ,_), (symbol', tokenName, amount)] -> symbol' == cSymbol && sym == Ada.adaSymbol
            _                                           -> False

        counterSpent :: Bool
        counterSpent = case (filter (\input -> counterInInput $ txInInfoResolved input) $ txInfoInputs info) of
            [n] -> True
            _   -> False

        {-burned :: Bool
        burned = case flattenValue (txInfoMint info) of
            [(sym1, _ , amt1), (sym2, _ , _)] -> (amt1 == -1 && sym1 == ownCurrencySymbol ctx) && sym1 /= sym2
            _                                 -> traceError "Invalid burn"-}

-- Compiles the validator
validator :: NFTCounterParams -> Scripts.Validator
validator cp = mkValidatorScript $
    $$(PlutusTx.compile [|| \cp' -> Scripts.wrapValidator $ mkValidator cp' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode cp

nftValidatorPlutusScript :: NFTCounterParams -> Script
nftValidatorPlutusScript cp = unValidatorScript $ validator cp

nftValidator :: NFTCounterParams -> Validator
nftValidator cp = Validator $ nftValidatorPlutusScript cp

nftValidatorScriptAsCbor :: NFTCounterParams -> LB.ByteString
nftValidatorScriptAsCbor cp = serialise $ nftValidator cp

nftValidatorScriptShortBs :: NFTCounterParams -> SBS.ShortByteString
nftValidatorScriptShortBs cp = SBS.toShort . LB.toStrict $ nftValidatorScriptAsCbor cp

nftValidatorScript :: NFTCounterParams -> PlutusScript PlutusScriptV1
nftValidatorScript cp = PlutusScriptSerialised . SBS.toShort . LB.toStrict $ nftValidatorScriptAsCbor cp


-- Compiles the policy
policy :: CurrencySymbol -> Scripts.MintingPolicy
policy cSymbol = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \cSymbol' -> Scripts.wrapMintingPolicy $ mkNFTPolicy cSymbol' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode cSymbol

nftPolicyPlutusScript :: CurrencySymbol -> Script
nftPolicyPlutusScript cSymbol = unMintingPolicyScript $ policy cSymbol

nftPolicy :: CurrencySymbol -> MintingPolicy
nftPolicy cSymbol = MintingPolicy $ nftPolicyPlutusScript cSymbol

nftPolicyScriptAsCbor :: CurrencySymbol -> LB.ByteString
nftPolicyScriptAsCbor cSymbol = serialise $ nftPolicy cSymbol

nftPolicyScriptShortBs :: CurrencySymbol -> SBS.ShortByteString
nftPolicyScriptShortBs cSymbol = SBS.toShort . LB.toStrict $ nftPolicyScriptAsCbor cSymbol

nftPolicyScript :: CurrencySymbol -> PlutusScript PlutusScriptV1
nftPolicyScript cSymbol = PlutusScriptSerialised . SBS.toShort . LB.toStrict $ nftPolicyScriptAsCbor cSymbol

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

module Botanica.NFTPolicy
(
  nftPolicyScript,
  nftPolicyScriptShortBs,
  
) where

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import qualified Ledger.Ada               as Ada
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)


{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: CurrencySymbol -> ValidatorHash -> PubKeyHash -> Integer -> ScriptContext -> Bool
mkNFTPolicy aSymbol vh pkh r ctx = txSignedByApp && 
                            (if r PlutusTx.Prelude.== 0 then checkRedeemedAmount
                            else checkRemintedAmount)
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx
        
        getAccessorTokenName :: BuiltinByteString
        getAccessorTokenName = case scriptOutputsAt vh info of
            [out] ->  case flattenValue $ snd out of
                [(sym1, _ , _), (sym2, tn2, _)] | sym1 == Ada.adaSymbol && sym2 == aSymbol -> unTokenName tn2
                _                                      -> traceError ""
            _     -> traceError ""

        -- Ensure 1 NFT is minted
        checkMintedAmount :: Value -> BuiltinByteString -> Bool
        checkMintedAmount val tokenNameBS = case flattenValue $ val of
            [(symbol', tokenName', amount)] -> symbol' == ownCurrencySymbol ctx && 
                                             unTokenName tokenName' == tokenNameBS && 
                                             amount == 1
            _                               -> False

        -- 1 token of given policy is burned and 1 token of given policy is reminted
        -- The suffixes of the token names are flipped
        checkRemintedAmount :: Bool
        checkRemintedAmount = case flattenValue $ txInfoMint info of
            [(sym1, tn1, a1), (sym2, tn2, a2)] -> sym1 == ownCurrencySymbol ctx &&
                                                sym2 == ownCurrencySymbol ctx &&
                                             ((a1 == -1 &&
                                             unTokenName tn2 == (flippedSuffix $ unTokenName tn1) &&
                                             a2 == 1) ||
                                             (a2 == -1 &&
                                             unTokenName tn1 == (flippedSuffix $ unTokenName tn2) &&
                                             a1 == 1))
            _                               -> False

        flippedSuffix :: BuiltinByteString -> BuiltinByteString
        flippedSuffix tn = appendByteString namePrefix flipSuffix
            where
                namePrefix = takeByteString tokenNameMaxIndex tn
                nameSuffix = dropByteString tokenNameMaxIndex tn

                flipSuffix :: BuiltinByteString
                flipSuffix = if nameSuffix == "A" then "B" else "A"

                tokenNameMaxIndex :: Integer
                tokenNameMaxIndex = (lengthOfByteString tn) - 1

        checkRedeemedAmount :: Bool
        checkRedeemedAmount = checkMintedAmount (txInfoMint info) newTokenName
            where
                newTokenName = appendByteString getAccessorTokenName "A"

        txSignedByApp :: Bool
        txSignedByApp = txSignedBy info $ pkh

policy :: CurrencySymbol -> ValidatorHash -> PubKeyHash -> Scripts.MintingPolicy
policy cs vh pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \cs' vh' pkh' -> Scripts.wrapMintingPolicy $ mkNFTPolicy cs' vh' pkh' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode cs
    `PlutusTx.applyCode`
    PlutusTx.liftCode vh
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

nftPolicyPlutusScript :: CurrencySymbol -> ValidatorHash -> PubKeyHash -> Script
nftPolicyPlutusScript cs vh pkh = unMintingPolicyScript $ policy cs vh pkh

nftPolicy :: CurrencySymbol -> ValidatorHash -> PubKeyHash -> MintingPolicy
nftPolicy cs vh pkh = MintingPolicy $ nftPolicyPlutusScript cs vh pkh

nftPolicyScriptAsCbor :: CurrencySymbol -> ValidatorHash -> PubKeyHash -> LB.ByteString
nftPolicyScriptAsCbor cs vh pkh = serialise $ nftPolicy cs vh pkh

nftPolicyScriptShortBs :: CurrencySymbol -> ValidatorHash -> PubKeyHash -> SBS.ShortByteString
nftPolicyScriptShortBs cs vh pkh = SBS.toShort . LB.toStrict $ nftPolicyScriptAsCbor cs vh pkh

nftPolicyScript :: CurrencySymbol -> ValidatorHash -> PubKeyHash -> PlutusScript PlutusScriptV1
nftPolicyScript cs vh pkh = PlutusScriptSerialised . SBS.toShort . LB.toStrict $ nftPolicyScriptAsCbor cs vh pkh
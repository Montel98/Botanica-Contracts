{-|
Module      : plutus-horrocubes-tokens.
Description : Application to generate NFTs using smart contracts.
License     : Apache-2.0
Maintainer  : angel.castillob@protonmail.com
Stability   : experimental
-}

-- LANGUAGE EXTENSIONS --------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

-- IMPORTS --------------------------------------------------------------------

import Cardano.Api                         hiding (TxId)
import Data.String                         (IsString (..))
import Ledger
import Ledger.Bytes                        (getLedgerBytes)
import Prelude
import System.Environment                  (getArgs)
import Data.Hex
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Data.ByteString.Short    as SBS

import Botanica.NFTPolicy

-- DEFINITIONS ----------------------------------------------------------------

-- | Application entry point.
-- The user must provide three arguments: UTXO id, the token name and the output path.
main :: IO ()
main = do
    [policyId, vh, pkh, policyFilePath] <- getArgs
    let symbol      = parseTokenSymbol policyId
        burnHash           = parseValidatorHash vh
        appKeyHash         = parsePubKeyHash pkh

    nftPolicyResult <- writeFileTextEnvelope policyFilePath Nothing $ nftPolicyScript symbol burnHash appKeyHash
    case nftPolicyResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote NFT policy to file " ++ policyFilePath

    writePlutusScript $ nftPolicyScriptShortBs symbol burnHash appKeyHash

parseValidatorHash :: String -> ValidatorHash
parseValidatorHash vh = Plutus.ValidatorHash (getLedgerBytes $ fromString $ vh)

parseTokenSymbol :: String -> CurrencySymbol
parseTokenSymbol symbol = Plutus.CurrencySymbol { Plutus.unCurrencySymbol = getLedgerBytes $ fromString $ symbol}

parsePubKeyHash :: String -> PubKeyHash
parsePubKeyHash pkh = Plutus.PubKeyHash { Plutus.getPubKeyHash = getLedgerBytes $ fromString $ pkh}

-- | Displays the execution budget.
writePlutusScript :: SBS.ShortByteString -> IO ()
writePlutusScript scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS []
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
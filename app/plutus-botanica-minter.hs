{-# LANGUAGE OverloadedStrings #-}

import Cardano.Api                         hiding (TxId)
import Data.String                         (IsString (..))
import Ledger
import Ledger.Bytes                        (getLedgerBytes)
import Prelude
import System.Environment                  (getArgs)
import Data.Hex
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Data.ByteString.Short    as SBS

import Botanica.CounterValidator

-- Entry point
main :: IO ()
main = do
    [policyId, policyFilePath] <- getArgs
    let counterSymbol      = parseTokenSymbol policyId

    nftPolicyResult <- writeFileTextEnvelope policyFilePath Nothing $ nftPolicyScript counterSymbol
    case nftPolicyResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote NFT policy to file " ++ policyFilePath

    writePlutusScript $ nftPolicyScriptShortBs counterSymbol

parseTokenName :: String -> TokenName
parseTokenName tn = Plutus.TokenName { Plutus.unTokenName = getLedgerBytes $ fromString $ hex tn }

parseTokenSymbol :: String -> CurrencySymbol
parseTokenSymbol symbol = Plutus.CurrencySymbol { Plutus.unCurrencySymbol = getLedgerBytes $ fromString $ symbol}

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
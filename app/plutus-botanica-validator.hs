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
import           PlutusTx.Prelude         hiding (Semigroup (..), unless, ($), error, (++))

import Botanica.CounterValidator

-- Entry point
main :: IO ()
main = do
    [policyId, nftSymbol', pkh', validatorFilePath] <- getArgs
    let counterSymbol      = parseTokenSymbol policyId
        nftSymbol          = parseTokenSymbol nftSymbol'
        keyHash            = parsePubKeyHash pkh'

    nftValidatorResult <- writeFileTextEnvelope validatorFilePath Nothing $ nftValidatorScript $ NFTCounterParams counterSymbol nftSymbol keyHash
    case nftValidatorResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote NFT Validator to file " ++ validatorFilePath

    writePlutusScript $ nftValidatorScriptShortBs $ NFTCounterParams counterSymbol nftSymbol keyHash

parseTokenName :: String -> TokenName
parseTokenName tn = Plutus.TokenName { Plutus.unTokenName = getLedgerBytes $ fromString $ hex tn }

parseTokenSymbol :: String -> CurrencySymbol
parseTokenSymbol symbol = Plutus.CurrencySymbol { Plutus.unCurrencySymbol = getLedgerBytes $ fromString $ symbol }

parsePubKeyHash :: String -> PubKeyHash
parsePubKeyHash pkh = Plutus.PubKeyHash { Plutus.getPubKeyHash = getLedgerBytes $ fromString $ pkh }

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
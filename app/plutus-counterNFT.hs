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

import Botanica.CounterNFT

-- Entry point
main :: IO ()
main = do
    [utxo', tokenName', filePath] <- getArgs
    let utxo      = parseUTxO utxo'
        tokenName = parseTokenName tokenName'


    nftPolicyResult <- writeFileTextEnvelope filePath Nothing $ mintScript tokenName utxo
    case nftPolicyResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote NFT policy to file " ++ filePath

    writePlutusScript $ nftScriptShortBs tokenName utxo

parseUTxO :: String -> TxOutRef
parseUTxO s =
  let
    (x, y) = span (/= '#') s
  in
    TxOutRef (TxId $ getLedgerBytes $ fromString x) $ read $ tail y

parseTokenName :: String -> TokenName
parseTokenName tn = Plutus.TokenName { Plutus.unTokenName = getLedgerBytes $ fromString $ hex tn }

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
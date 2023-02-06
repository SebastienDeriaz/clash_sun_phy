module CsvData where

import Parsers
import Clash.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V

import Data.FileEmbed (embedFile)
import Data.Maybe (catMaybes)

import Data.Csv (decodeByName)

l1Data :: [HexOctetSample]
l1Data = case decodeByName (LBS.fromStrict $(embedFile "test/L.1.csv")) of
    Left err -> error err
    Right (_, samples) -> V.toList samples

type L1Length = 100

l14Data :: [ComplexSample]
l14Data = case decodeByName (LBS.fromStrict $(embedFile "test/L.14.csv")) of
    Left err -> error err
    Right (_, samples) -> V.toList samples
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module IeeeAnnexLSpec where

import Clash.Prelude hiding (undefined)
import Prelude

import MrOfdmModulatorTester
import Parsers
import SunPhy.Serializer

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.FileEmbed (embedFile)
import Data.Maybe (catMaybes)

-- import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Debug.Trace

import Data.Csv (decodeByName)
import Test.Hspec
import Test.QuickCheck

-- :(
-- instance FromField Rational where
--    parseField s = pure $ read @Rational $ trace ("PARSING: " <> T.unpack (T.decodeUtf8 s)) $ T.unpack (T.decodeUtf8 s)

-- TODO : Convert hex octet sample to bit stream

l1Data :: [HexOctetSample]
l1Data = case decodeByName (LBS.fromStrict $(embedFile "test/L.1.csv")) of
    Left err -> error err
    Right (_, samples) -> V.toList samples

l14Data :: [ComplexSample]
l14Data = case decodeByName (LBS.fromStrict $(embedFile "test/L.14.csv")) of
    Left err -> error err
    Right (_, samples) -> V.toList samples

type DataCount = 4
type DataType = Unsigned 4

type Input = MrOfdmModulatorTesterInput DataCount DataType
type Output = MrOfdmModulatorTesterOutput DataType

inputs :: [Input]
inputs = []

outputs :: [Output]
outputs = []

ieeeAnnexLSpec :: Spec
ieeeAnnexLSpec = do
    traceM $ show l1Data
    traceM $ show l14Data
    describe
        "IEEE 802.15.4g-2012 Annex L"
        $ it
            "consumes message L.1 and produces IQ signal L.14"
            True

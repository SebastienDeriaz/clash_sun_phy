{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Parsers where

import Clash.Prelude
import Data.Csv
import Data.Complex
import Numeric (readHex)

newtype HexOctetSample = HexOctetSample {value :: Unsigned 8}
    deriving (Show)

instance FromNamedRecord HexOctetSample where
    parseNamedRecord r = do
        valueString :: String <- r .: "Value (hex)"
        value <- case readHex valueString of
            [(n, "")] -> pure $ toEnum n
            _ -> fail "could not parse hex value"
        pure $ HexOctetSample {..}

newtype ComplexSample = ComplexSample {value :: Complex Float}
    deriving (Show)

instance FromNamedRecord ComplexSample where
    parseNamedRecord r = do
        real <- r .: "Real"
        imag <- r .: "Imaginary"
        let value = real :+ imag
        pure $ ComplexSample {..}
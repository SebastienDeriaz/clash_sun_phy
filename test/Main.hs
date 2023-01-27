module Main where

import Test.Hspec (hspec)
import Prelude

import IeeeAnnexLSpec
import SerializerSpec

main :: IO ()
main = hspec $ do
    serializerSpec
    ieeeAnnexLSpec

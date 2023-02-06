module Main where

import Test.Hspec (hspec)
import Prelude

import MrOfdmModulatorSpec
import SerializerSpec

main :: IO ()
main = hspec $ do
    serializerSpec
    mrOfdmModulatorSpec

    

module SerializerSpec where

import Test.Hspec
import Test.QuickCheck

import SunPhy.Serializer

import Clash.Prelude hiding (repeat, take)
import Prelude (repeat, take)

type DataCount = 4
type DataType = Unsigned 4

type Input = SerializerInput DataCount DataType
type Output = SerializerOutput DataType

inputs :: [Input]
inputs = i <$> [0 ..]
    where
        i n =
            SerializerInput
                { inputData = 5 :> 6 :> 7 :> 8 :> Nil
                , inputStart = inputStart n
                , inputReady = 1
                }
        inputStart 0 = 1
        inputStart _ = 0

outputs :: [Output]
outputs = o <$> [0 ..]
    where
        o n
            | n > 0 && n < 5 =
                SerializerOutput
                    { outputData = n - 1 + 5
                    , outputReady = 0
                    , outputValid = 1
                    , outputLast = boolToBit (n == 4)
                    }
            | otherwise =
                SerializerOutput
                    { outputData = 0
                    , outputReady = 1
                    , outputValid = 0
                    , outputLast = 0
                    }

firstNShouldBe :: (Eq a, Show a) => Int -> [a] -> [a] -> Expectation
firstNShouldBe n xs ys = take n xs `shouldBe` take n ys

serializerSpec :: Spec
serializerSpec =
    describe "Serializer" $
        it "serializes input data as an output Axi string" $ do
            let expected = outputs
            let actual = simulate @System serializer inputs
            let samples = 10
            firstNShouldBe samples actual expected

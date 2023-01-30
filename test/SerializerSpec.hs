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
                { _data = 5 :> 6 :> 7 :> 8 :> Nil
                , start = start n
                , ready = 1
                }
        start 0 = 1
        start _ = 0

outputs :: [Output]
outputs = o <$> [0 ..]
    where
        o n
            | n > 0 && n < 5 =
                SerializerOutput
                    { _data = n - 1 + 5
                    , ready = 0
                    , valid = 1
                    , last = boolToBit (n == 4)
                    }
            | otherwise =
                SerializerOutput
                    { _data = 0
                    , ready = 1
                    , valid = 0
                    , last = 0
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

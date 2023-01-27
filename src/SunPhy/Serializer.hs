{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SunPhy.Serializer where
    --(serializer,
    --SerializerInput,
    --SerializerOutput)
    --where 

import Clash.Prelude
import Data.Functor ((<&>))

-- A module to convert a vector to an AXI serial stream

data State
    = Idle
    | Running
    deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
    deriving anyclass (NFDataX)

data SerializerInput n a = SerializerInput
    { inputData :: Vec n a
    , inputStart :: Bit
    , inputReady :: Bit
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data SerializerOutput a = SerializerOutput
    { outputData :: a
    , outputReady :: Bit
    , outputValid :: Bit
    , outputLast :: Bit
    }
    deriving stock (Generic, Show)
    deriving anyclass (NFDataX)

instance Eq a => Eq (SerializerOutput a) where
    o1 == o2 =
        (outputReady o1 == outputReady o2)
            && (outputValid o1 == outputValid o2)
            && (outputLast o1 == outputLast o2)
            && (outputValid o1 == 0 || outputData o1 == outputData o2)

serializer
    :: forall a n dom
     . HiddenClockResetEnable dom
    => KnownNat n
    => NFDataX a
    => Signal dom (SerializerInput n a)
    -> Signal dom (SerializerOutput a)
serializer input = do
    outputReady <- boolToBit <$> (state .==. pure Idle)
    outputValid <- outputValid
    outputData <- (!!) <$> (inputData <$> input) <*> bitCounter
    outputLast <- boolToBit <$> (state .==. pure Running .&&. bitCounterEnd .==. 1)
    pure $ SerializerOutput {..}
    where
        state =
            register Idle $
                nextState
                    <$> state
                    <*> (inputStart <$> input)
                    <*> slaveWrite
                    <*> bitCounterEnd

        outputValid = boolToBit <$> (state .==. pure Running)

        slaveWrite = outputValid * (inputReady <$> input)

        len = length <$> (inputData <$> input)

        bitCounter :: Signal dom (Index n)
        bitCounter = register (0 :: Index n) (nextBitCounter <$> state <*> slaveWrite <*> bitCounterEnd <*> bitCounter)

        bitCounterEnd :: Signal dom Bit
        bitCounterEnd = boolToBit <$> (bitCounter .==. (fromInteger <$> (fromIntegral <$> (len - 1))))

nextState :: State -> Bit -> Bit -> Bit -> State
--        ┌state
--        │       ┌start_i
--        │       │ ┌slaveWrite
--        │       │ │ ┌bitCounterEnd
--        │       │ │ │
nextState Idle 1 _ _ = Running
nextState Running _ 1 1 = Idle
nextState x _ _ _ = x

nextBitCounter :: KnownNat n => State -> Bit -> Bit -> Index n -> Index n
--             ┌state
--             │        ┌slaveWrite
--             │        │ ┌bitCounterEnd
--             │        │ │ ┌bitCounter
-- Idle        │        │ │ │
nextBitCounter Idle _ _ _ = 0
nextBitCounter Running 1 1 _ = 0
nextBitCounter Running 1 0 x = x + 1
nextBitCounter _ _ _ x = x

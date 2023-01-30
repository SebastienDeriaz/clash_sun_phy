{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module SunPhy.Serializer where

import Clash.Prelude
import Data.Functor ((<&>))

-- A module to convert a vector to an AXI serial stream

data State
    = Idle
    | Running
    deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
    deriving anyclass (NFDataX)

data SerializerInput n a = SerializerInput
    { _data :: Vec n a
    , start :: Bit
    , ready :: Bit
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data SerializerOutput a = SerializerOutput
    { _data :: a
    , ready :: Bit
    , valid :: Bit
    , last :: Bit
    }
    deriving stock (Generic, Show)
    deriving anyclass (NFDataX)

instance Eq a => Eq (SerializerOutput a) where
    o1 == o2 =
        (o1.ready == o2.ready)
            && (o1.valid == o2.valid)
            && (o1.last == o2.last)
            && (o1.valid == 0 || o1._data == o2._data)

serializer
    :: forall a n dom
     . HiddenClockResetEnable dom
    => KnownNat n
    => NFDataX a
    => Signal dom (SerializerInput n a)
    -> Signal dom (SerializerOutput a)
serializer input = do
    ready <- boolToBit <$> (state .==. pure Idle)
    valid <- outputValid
    _data <- (!!) <$> (input <&> (._data)) <*> bitCounter
    last <- boolToBit <$> (state .==. pure Running .&&. bitCounterEnd .==. 1)
    pure $ SerializerOutput {..}
    where
        state =
            register Idle $
                nextState
                    <$> state
                    <*> (input <&> (.start))
                    <*> slaveWrite
                    <*> bitCounterEnd

        outputValid = boolToBit <$> (state .==. pure Running)

        slaveWrite = outputValid * (input <&> (.ready))

        len = length <$> (input <&> (._data))

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
nextState Idle    1 _ _ = Running
nextState Running _ 1 1 = Idle
nextState x       _ _ _ = x

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

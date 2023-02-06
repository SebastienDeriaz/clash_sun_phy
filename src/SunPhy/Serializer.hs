module SunPhy.Serializer where

import Clash.Prelude
import Data.Functor ((<&>))
import SunPhy.AXI
import SunPhy.AXI (AxiBackward(AxiBackward))

-- A module to convert a vector to an AXI serial stream

data State
    = Idle
    | Running
    deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
    deriving anyclass (NFDataX)

data SerializerInput n a = SerializerInput
    { dataVec :: Vec n a
    , start :: Bit
    , axiOutputFeedback :: AxiBackward
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data SerializerOutput a = SerializerOutput
    { axiOutput :: AxiForward a
    , axiInputFeedback :: AxiBackward
    }
    deriving stock (Generic, Show)
    deriving anyclass (NFDataX)

instance Eq a => Eq (SerializerOutput a) where
    o1 == o2 = (o1.axiInputFeedback.ready == o2.axiInputFeedback.ready)
        && (o1.axiOutput.valid == o2.axiOutput.valid)
        && (o1.axiOutput.last == o2.axiOutput.last)
        && (o1.axiOutput.valid == 0 || o1.axiOutput._data == o2.axiOutput._data)

serializer
    :: forall a n dom
     . HiddenClockResetEnable dom
    => KnownNat n
    => NFDataX a
    => Signal dom (SerializerInput n a)
    -> Signal dom (SerializerOutput a)
serializer input = do
    axiOutput <- do
        valid <- outputValid
        _data <- (!!) <$> (input <&> (.dataVec)) <*> bitCounter
        last <- boolToBit <$> (state .==. pure Running .&&. bitCounterEnd .==. 1)
        pure AxiForward {..}
    axiInputFeedback <- do
        ready <- boolToBit <$> (state .==. pure Idle)
        pure AxiBackward {..}
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

        slaveWrite = outputValid * (input <&> (.axiOutputFeedback) <&> (.ready))

        len = length <$> (input <&> (.dataVec))

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

module SunPhy.Serializer where

import Clash.Prelude

-- A module to convert a vector to an AXI serial stream

data State = Idle
           | Running
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX

serializer
  :: forall dom a n . (HiddenClockResetEnable dom, NFDataX a, KnownNat n)
  => Signal dom (Vec n (a))
  -> Signal dom Bit -- start_i
  -> Signal dom Bit -- ready_i
  -> Signal dom (Bit, Bit, a, Bit) -- ready_o, valid_o, data_o, last_o
serializer vec start_i ready_i = bundle(ready_o, valid_o, data_o, last_o)
  where
    slaveWrite = valid_o * ready_i

    ready_o = boolToBit <$> (state .==. pure Idle)

    len = length <$> vec

    nextState :: State -> Bit -> Bit -> Bit -> State
    --        ┌state
    --        │       ┌start_i
    --        │       │ ┌slaveWrite
    --        │       │ │ ┌bitCounterEnd
    --        │       │ │ │
    nextState Idle    1 _ _ = Running
    nextState Running _ 1 1 = Idle
    nextState x       _ _ _ = x

    state = register (Idle) $ nextState <$> state <*> start_i <*> slaveWrite <*> bitCounterEnd

    nextBitCounter :: State -> Bit -> Bit -> Index n -> Index n
    --             ┌state
    --             │        ┌slaveWrite
    --             │        │ ┌bitCounterEnd
    --             │        │ │ ┌bitCounter
    -- Idle        │        │ │ │
    nextBitCounter Idle     _ _ _ = 0
    nextBitCounter Running  1 1 _ = 0
    nextBitCounter Running  1 0 x = x + 1
    nextBitCounter _        _ _ x = x

    bitCounter :: Signal dom (Index n)
    bitCounter = register (0 :: Index n) (nextBitCounter <$> state <*> slaveWrite <*> bitCounterEnd <*> bitCounter)
    bitCounterEnd = boolToBit <$> (bitCounter .==. (fromInteger <$> (fromIntegral <$> (len - 1))))

    last_o = boolToBit <$> (state .==. pure Running .&&. bitCounterEnd .==. 1)

    dataOut :: Vec n (a) -> Index n -> a 
    dataOut v i = v !! i

    data_o = dataOut <$> vec <*> bitCounter
    valid_o :: Signal dom Bit
    valid_o = boolToBit <$> (state .==. pure Running)
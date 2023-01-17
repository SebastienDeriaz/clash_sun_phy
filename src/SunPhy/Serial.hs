module SunPhy.Serial where

import Clash.Prelude

-- A module to convert a vector to an AXI serial stream

data State = Idle
           | Running
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX

serial
  :: forall dom a n . (HiddenClockResetEnable dom, NFDataX a, KnownNat n)
  => Signal dom (Vec n (a))
  -> Signal dom a -- valid_i
  -> Signal dom a -- ready_i
  -> Signal dom (Bit, a, Bit) -- valid_o, data_o, last_o
serial vec valid_i ready_i = bundle(valid_o, data_o, last_o)
  where
    slaveWrite = valid_o * ready_i

    nextState :: State -> Bit -> Bit -> Bit -> State
    --        ┌state
    --        │       ┌valid_i 
    --        │       │ ┌slaveWrite
    --        │       │ │ ┌bitCounterEnd
    --        │       │ │ │
    nextState Idle    1 _ _ = Running
    nextState Running _ 1 1 = Idle
    nextState x       _ _ _ = x

    state = register (Idle) $ nextState <$> state <*> valid_i <*> slaveWrite <*> bitCounterEnd

    nextBitCounter :: State -> Bit -> Bit -> Bit -> Index n -> Index n
    --             ┌state
    --             │       ┌valid_i 
    --             │       │ ┌slaveWrite
    --             │       │ │ ┌bitCounterEnd
    --             │       │ │ │ ┌bitCounter
    -- Idle        │       │ │ │ │
    nextBitCounter Idle    _ _ _ _ = 0
    nextBitCounter Running _ 1 1 _ = 0
    nextBitCounter Running _ 1 0 x = x + 1
    nextBitCounter _       _ _ _ x = x

    bitCounter = register (0 :: Index n) (nextBitCounter <$> state <*> valid_i <*> slaveWrite <*> bitCounterEnd <*> bitCounter)
    bitCounterEnd = boolToBit <$> (bitCounter .==. (n - 1))

    last_o = boolToBit <$> (bitCounterEnd .==. 1)

    dataOut :: Vec n (a) -> Index n -> a 
    dataOut v i = v !! i

    data_o = dataOut <$> vec <*> bitCounter
    valid_o = boolToBit <$> (state .==. pure Running)
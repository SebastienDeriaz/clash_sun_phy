{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module SunPhy.MR_FSK.PSDU where

import Clash.Prelude

data State = Idle
           | Running
           | Finish
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX


nextState :: State -> Bit -> Bit -> State
--        ┌state
--        │       ┌ready 
--        │       │ ┌last
-- Idle   │       │ │
nextState Idle    0 _ = Idle
nextState Idle    1 _ = Running
nextState Running 1 1 = Finish
nextState Running _ _ = Running
nextState Finish  1 _ = Finish
nextState Finish  0 _ = Idle

nextBitIndex :: State -> Bit -> Unsigned 5 -> Unsigned 5
-- state, ready, bitIndex
--           ┌state
--           │       ┌ready 
--           │       │ ┌bitIndex
-- Idle      │       │ │
nextBitIndex Running 0 x = x
nextBitIndex Running 1 x = x + 1
nextBitIndex Idle    1 x = x + 1
nextBitIndex _       _ x = 0


psdu
  :: forall dom . (HiddenClockResetEnable dom)
  => Signal dom Bit -- ready_i
  -> Signal dom (Bit, Bit, Bit) -- valid_o, data_o, last_o
psdu ready_i = bundle(valid_o, data_o, last_o)
  where
    state = register Idle (nextState <$> state <*> ready_i <*> last_o)

    bitIndex = register (0 :: Unsigned 5) (nextBitIndex <$> state <*> ready_i <*> bitIndex)
    bitIndexEnd = boolToBit <$> (bitIndex .==. bitIndexMax)

    --bitIndexMax :: Signal dom (Unsigned 4)
    bitIndexMax = pure (0b11111 :: Unsigned 5)

    psdu_data = pure (0b1111_0000_1111_0000_1111_0000_1111_0011 :: BitVector 32)

    -- End of the transmission
    -- end=0 when start == 1
    -- end=1 when bitIndex == 15
    last_o = boolToBit <$> (bitIndex .==. bitIndexMax)

    valid_o = pure 1
    data_o = boolToBit <$> (testBit <$> psdu_data <*> (fromIntegral <$> bitIndex))
    
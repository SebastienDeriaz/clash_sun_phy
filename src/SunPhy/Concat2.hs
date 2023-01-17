{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module SunPhy.Concat2 where

import Clash.Prelude

-- State machine
data State = A
           | B
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX




nextState :: State -> Bit -> Bit -> Bit -> State
-- state, slaveWrite, a_last_i, b_last_i
nextState A 1 1 _ = B -- to B if the slave did write and a_last_i is 1
nextState B 1 _ 1 = A -- to A if the slave did write and b_last_i is 1
nextState x _ _ _ = x
concat2
    :: forall dom . (HiddenClockResetEnable dom)
    => Signal dom Bit -- a_valid
    -> Signal dom Bit -- a_data
    -> Signal dom Bit -- a_last
    -> Signal dom Bit -- b_valid
    -> Signal dom Bit -- b_data
    -> Signal dom Bit -- b_last
    -> Signal dom Bit -- ready (input)
    -> Signal dom (Bit, Bit, Bit, Bit, Bit) -- a_ready, b_ready, valid, data, last
concat2 a_valid_i a_data_i a_last_i b_valid_i b_data_i b_last_i ready_i = bundle (a_ready_o, b_ready_o, valid_o, data_o, last_o)
  where
    slaveWrite = valid_o * ready_i

    state = register A (nextState <$> state <*> slaveWrite <*> a_last_i <*> b_last_i)

    a_ready_o = ready_i * (boolToBit <$> (state .==. pure A))
    b_ready_o = ready_i * (boolToBit <$> (state .==. pure B))

    valid_o = mux (state .==. pure A) a_valid_i b_valid_i
    
    last_o = b_last_i
    data_o = mux (state .==. pure A) a_data_i b_data_i
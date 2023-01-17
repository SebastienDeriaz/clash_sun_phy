{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module SunPhy.Concat3 where

import Clash.Prelude

-- State machine
data State = A
           | B
           | C
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX

nextState :: State -> Bit -> Bit -> Bit -> Bit -> State
-- state, slaveWrite, a_last_i, b_last_i, c_last_i
nextState A 1 1 _ _ = B -- to B if the slave did write and a_last_i is 1
nextState B 1 _ 1 _ = C -- to C if the slave did write and b_last_i is 1
nextState C 1 _ _ 1 = A -- to A if the slave did write and c_last_i is 1
nextState x _ _ _ _ = x


mux3 :: State -> Bit -> Bit -> Bit -> Bit
mux3 state a b c = case state of
    A -> a
    B -> b
    C -> c

concat3
    :: forall dom . (HiddenClockResetEnable dom)
    => Signal dom Bit -- a_valid_i
    -> Signal dom Bit -- a_data_i
    -> Signal dom Bit -- a_last_i
    -> Signal dom Bit -- b_valid_i
    -> Signal dom Bit -- b_data_i
    -> Signal dom Bit -- b_last_i
    -> Signal dom Bit -- c_valid_i
    -> Signal dom Bit -- c_data_i
    -> Signal dom Bit -- c_last_i
    -> Signal dom Bit -- ready_i
    -> Signal dom (Bit, Bit, Bit, Bit, Bit, Bit) -- a_ready_o, b_ready_o, c_ready_o valid, data, last
concat3 a_valid_i a_data_i a_last_i b_valid_i b_data_i b_last_i c_valid_i c_data_i c_last_i ready_i = bundle (a_ready_o, b_ready_o, c_ready_o, valid_o, data_o, last_o)
  where
    slaveWrite = valid_o * ready_i

    state = register A (nextState <$> state <*> slaveWrite <*> a_last_i <*> b_last_i <*> c_last_i)

    a_ready_o = ready_i * (boolToBit <$> (state .==. pure A))
    b_ready_o = ready_i * (boolToBit <$> (state .==. pure B))
    c_ready_o = ready_i * (boolToBit <$> (state .==. pure C))

    valid_o = mux3 <$> state <*> a_valid_i <*> b_valid_i <*> c_valid_i
    
    last_o = c_last_i
    data_o = mux3 <$> state <*> a_data_i <*> b_data_i <*> c_data_i
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module SunPhy.Concat4 where

import Clash.Prelude
import SunPhy.MR_OFDM.Constants

-- State machine
data State = A
           | B
           | C
           | D
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX

nextState :: State -> Bit -> Bit -> Bit -> Bit -> Bit -> State
-- state, slaveWrite, a_last_i, b_last_i, c_last_i
nextState A 1 1 _ _ _ = B -- to B if the slave did write and a_last_i is 1
nextState B 1 _ 1 _ _ = C -- to C if the slave did write and b_last_i is 1
nextState C 1 _ _ 1 _ = D -- to D if the slave did write and c_last_i is 1
nextState D 1 _ _ _ 1 = A -- to A if the slave did write and d_last_i is 1
nextState x _ _ _ _ _ = x


mux4 :: NFDataX a => State -> a -> a -> a -> a -> a
mux4 state a b c d = case state of
    A -> a
    B -> b
    C -> c
    D -> d

concat4
    :: forall dom a . (HiddenClockResetEnable dom)
    => NFDataX a
    => Signal dom Bit -- a_valid_i
    -> Signal dom a  -- a_data_i
    -> Signal dom Bit -- a_last_i
    -> Signal dom Bit -- b_valid_i
    -> Signal dom a  -- b_data_i
    -> Signal dom Bit -- b_last_i
    -> Signal dom Bit -- c_valid_i
    -> Signal dom a  -- c_data_i
    -> Signal dom Bit -- c_last_i
    -> Signal dom Bit -- d_valid_i
    -> Signal dom a  -- d_data_i
    -> Signal dom Bit -- d_last_i
    -> Signal dom Bit -- ready_i
    -> Signal dom (Bit, Bit, Bit, Bit, Bit, a, Bit, Unsigned 2) -- a_ready_o, b_ready_o, c_ready_o, d_ready_o, valid, data, last
concat4 a_valid_i a_data_i a_last_i b_valid_i b_data_i b_last_i c_valid_i c_data_i c_last_i d_valid_i d_data_i d_last_i ready_i = bundle (a_ready_o, b_ready_o, c_ready_o, d_ready_o, valid_o, data_o, last_o, state_num)
  where
    slaveWrite = valid_o * ready_i

    state = register A (nextState <$> state <*> slaveWrite <*> a_last_i <*> b_last_i <*> c_last_i <*> d_last_i)

    a_ready_o = ready_i * (boolToBit <$> (state .==. pure A))
    b_ready_o = ready_i * (boolToBit <$> (state .==. pure B))
    c_ready_o = ready_i * (boolToBit <$> (state .==. pure C))
    d_ready_o = ready_i * (boolToBit <$> (state .==. pure D))

    valid_o = mux4 <$> state <*> a_valid_i <*> b_valid_i <*> c_valid_i <*> d_valid_i
    
    last_o = d_last_i
    data_o = mux4 <$> state <*> a_data_i <*> b_data_i <*> c_data_i <*> d_data_i

    f_state_num :: State -> Unsigned 2
    f_state_num A = 0
    f_state_num B = 1
    f_state_num C = 2
    f_state_num D = 3
    
    state_num = f_state_num <$> state
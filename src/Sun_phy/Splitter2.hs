{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Sun_phy.Splitter2 where

import Clash.Prelude



data State = A
           | B
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX

-- sel = 1 -> A
-- sel = 0 -> B


nextState :: State -> Bool -> Bit -> Bit -> Bit -> State
-- state, counterEnd, last_i, aWrite, bWrite
nextState A True _ 1 _ = B
nextState B _    1 _ 1 = A
nextState x _    _ _ _ = x

nextCounter :: Bit -> Bit -> Bit -> Unsigned 10 -> Unsigned 10
-- aWrite, counterEnd, last
nextCounter 1 0 0 x = x + 1
nextCounter 1 0 1 x = x
nextCounter 1 1 _ _ = 0
nextCounter 0 _ _ x = x

splitter2
    :: forall dom . HiddenClockResetEnable dom
    => Signal dom Bit -- valid_i
    -> Signal dom Bit -- data_i
    -> Signal dom Bit -- last_i
    -> Signal dom Bit -- a_ready_i
    -> Signal dom Bit -- b_ready_i
    -> Signal dom (Unsigned 10) -- a_length
    -> Signal dom (Bit, Bit, Bit, Bit, Bit, Bit, Bit) -- ready_o, a_valid_o, a_data_o, a_last_o, b_valid_o, b_data_o, b_last_o
splitter2 valid_i data_i last_i a_ready_i b_ready_i a_length = bundle(ready_o, a_valid_o, a_data_o, a_last_o, b_valid_o, b_data_o, b_last_o)
  where
    aWrite = a_ready_i * a_valid_o
    bWrite = b_ready_i * b_valid_o

    counter = register (0 :: Unsigned 10) $ nextCounter <$> aWrite <*> (boolToBit <$> counterEnd) <*> last_i <*> counter

    counterEnd = counter .==. (a_length - 1)

    state = register A $ nextState'
    nextState' = nextState <$> state <*> counterEnd <*> last_i <*> aWrite <*> bWrite


    ready_o   = mux (state .==. pure A) a_ready_i b_ready_i
    a_valid_o = mux (state .==. pure A) valid_i   (pure 0) 
    a_data_o  = mux (state .==. pure A) data_i    (pure 0) 
    b_valid_o = mux (state .==. pure A) (pure 0)  valid_i
    b_data_o  = mux (state .==. pure A) (pure 0)  data_i

    a_last_o  = boolToBit <$> counterEnd
    b_last_o  = last_i
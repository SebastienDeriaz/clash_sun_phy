{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module SunPhy.Switch2 where

import Clash.Prelude


-- sel = 1 -> A
-- sel = 0 -> B

switch2
    :: forall dom . HiddenClockResetEnable dom
    => Signal dom Bit -- A_valid_i
    -> Signal dom Bit -- A_data_i
    -> Signal dom Bit -- A_last_i
    -> Signal dom Bit -- B_valid_i
    -> Signal dom Bit -- B_data_i
    -> Signal dom Bit -- B_last_i
    -> Signal dom Bit -- ready_i
    -> Signal dom Bit -- sel
    -> Signal dom (Bit, Bit, Bit, Bit, Bit) -- A_ready_o B_ready_o valid_o data_o last_o
switch2 a_valid_i a_data_i a_last_i b_valid_i b_data_i b_last_i ready_i sel = bundle(a_ready_o, b_ready_o, valid_o, data_o, last_o)
  where    
    selb = bitToBool <$> sel
    a_ready_o = mux selb ready_i (pure 0)
    b_ready_o = mux selb (pure 0) ready_i
    valid_o = mux selb a_valid_i b_valid_i
    data_o = mux selb a_data_i b_data_i
    last_o = mux selb a_last_i b_last_i

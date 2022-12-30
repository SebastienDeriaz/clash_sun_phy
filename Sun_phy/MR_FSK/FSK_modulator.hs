{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Sun_phy.MR_FSK.FSK_modulator where

import Clash.Prelude


samplesPerSymbol = 10

fsk_modulator
    :: forall dom . HiddenClockResetEnable dom
    => Signal dom Bit -- ready_i
    -> Signal dom Bit -- valid_i
    -> Signal dom Bit -- data_i
    -> Signal dom Bit -- last_i
    -> Signal dom (Bit, Bit, (SFixed 2 10), (SFixed 2 10), Bit) -- ready_o, valid_o, data_r_o, data_i_o, last_o
fsk_modulator ready_i valid_i data_i last_i = bundle(ready_o, valid_o, data_r_o, data_i_o, last_o)
  where
    sinAngle :: Signal dom (SFixed 2 10)
    sinAngle = pure ($$(fLit (sin 1.05)) :: SFixed 2 10)
    --sinAngle = pure (1.05 :: SFixed 2 10)

    data_r_o = sin <$> sinAngle
    data_i_o = pure 0.5 -- sin <$> angle


    ready_o = pure 0
    valid_o = pure 0
    last_o = pure 0
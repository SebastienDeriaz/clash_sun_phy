module Sun_phy.MR_OFDM.MR_OFDM_Modulator where

import Clash.Prelude


mrOfdmModulator
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Bit -- ...
  -> Signal dom Bit -- ready_i
  -> Signal dom Bit -- valid_i
  -> Signal dom Bit -- psdu_valid_i
  -> Signal dom Bit -- psdu_data_i
  -> Signal dom Bit -- psdu_last_i
  -> Signal dom (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit) -- ready_o, valid_o, data_o, last_o, psdu_ready_i, shr_ready_i, splitter2_a_ready_i, scrambler_ready_i, test, test 2
mrOfdmModulator
  -- Set scrambler seeds (See table 158 802.15.4g)
  
  -- Interleaver MCS can be different for PHR and Payload
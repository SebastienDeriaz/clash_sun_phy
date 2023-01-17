module Test_MR_OFDM_Modulator where


testMrOfdmModulator
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Unsigned 3 -- ofdmOption
  -> Signal dom Bit -- ready_i
  -> Signal dom Bit -- valid_i
  -> Signal dom Bit -- psdu_valid_i
  -> Signal dom Bit -- psdu_data_i
  -> Signal dom Bit -- psdu_last_i
  -> Signal dom (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit) -- ready_o, valid_o, data_o, last_o, psdu_ready_i
testMrOfdmModulator ofdmOption = bundle (ready_o, valid_o, data_o, last_o, psdu_ready_i)
  where
    vec = pure (listToVecTH [0,0,0,0,1,1,1,1])

    


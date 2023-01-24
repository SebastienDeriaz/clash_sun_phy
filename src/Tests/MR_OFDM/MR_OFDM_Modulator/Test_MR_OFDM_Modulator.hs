module Tests.MR_OFDM.MR_OFDM_Modulator.Test_MR_OFDM_Modulator where


import Clash.Prelude
import SunPhy.Serializer
import SunPhy.MR_OFDM.MR_OFDM_Modulator
import SunPhy.MR_OFDM.Constants

testMrOfdmModulator
  :: forall dom n . (HiddenClockResetEnable dom, KnownNat n)
  => Signal dom OFDM_Option
  -> Signal dom MCS
  -> Signal dom Bit -- phyOFDMInterleaving
  -> Signal dom (Vec n (Bit))
  -> Signal dom Bit -- start_i
  -> Signal dom Bit -- ready_i
  -> Signal dom (Bit, Bit, IQ, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Unsigned 4) -- ready_o, valid_o, data_o, last_o
testMrOfdmModulator ofdmOption mcs phyOFDMInterleaving inputVec start_i ready_i = bundle (ready_o, valid_o, data_o, last_o, test1, test2, test3, test4, test5, test6, ofdm_state)
  where
    (ready_o, psdu_valid_o, psdu_data_o, psdu_last_o) = unbundle $ serializer inputVec start_i psdu_ready_o

    psdu_length :: Signal dom (Unsigned 11)
    psdu_length = fromIntegral <$> ((div 8) <$> (length <$> inputVec))

    (psdu_ready_o, valid_o, data_o, last_o, test1, test2, test3, test4, test5, test6, ofdm_state) = unbundle $ mrOfdmModulator
      ofdmOption
      mcs
      phyOFDMInterleaving
      ready_i
      start_i
      psdu_valid_o
      psdu_data_o
      psdu_last_o
      psdu_length




    


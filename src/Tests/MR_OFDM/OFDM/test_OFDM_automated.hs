module Tests.MR_OFDM.MR_OFDM_Modulator.Test_OFDM_automated where


import Clash.Prelude
import SunPhy.Serializer hiding (State)
import SunPhy.MR_OFDM.OFDM
import SunPhy.MR_OFDM.Constants

testOFDMautomated
  :: forall dom n . (HiddenClockResetEnable dom, KnownNat n)
  => Signal dom OFDM_Option
  -> Signal dom MCS
  -> Signal dom Bit -- phyOFDMInterleaving
  -> Signal dom (Vec n (Bit))
  -> Signal dom Bit -- start_i
  -> Signal dom Bit -- ready_i
  -> Signal dom (Unsigned 4)
  -> Signal dom (BitVector 9)
  -> Signal dom Bit -- Write PN9 seed and pilot set
  -> Signal dom (Bit, Bit, IQ, Bit, State, Bit, Unsigned 7, Bit, Bit) -- ready_o, valid_o, data_o, last_o
testOFDMautomated ofdmOption mcs phyOFDMInterleaving inputVec start_i ready_i pilot_set pn9_seed pn9_pilot_write = bundle (ready_o, valid_o, data_o, last_o, state, psdu_ready_o * phr_valid, subcarrierCounter, subcarrierCounterEnd, pilotNext)
  where
    (ready_o, phr_valid, phr_data, phr_last) = unbundle $ serializer inputVec start_i psdu_ready_o

    (psdu_ready_o, valid_o, data_o, last_o, _, pn9_reg, state, subcarrierCounter, subcarrierCounterEnd, pilotNext) = unbundle $ ofdm
      ofdmOption
      mcs
      (pure 1)
      phr_valid
      phr_data
      phr_last
      pilot_set
      pn9_pilot_write
      pn9_seed
      pn9_pilot_write
      ready_i




    


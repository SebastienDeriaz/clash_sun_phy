module Tests.MR_FSK.MR_FSK_modulator.Top where

import Clash.Prelude

import Tests.MR_FSK.MR_FSK_modulator.PSDU
import SunPhy.MR_FSK.MR_FSK_modulator


-- This is a test module to try the MR_FSK modulator with a test sequence (from PSDU_test)

topEntity
  :: forall dom . (HiddenClockResetEnable dom)
  => Signal dom Bit -- macFCSType
  -> Signal dom Bit -- phyFSKScramblePSDU
  -> Signal dom Bit -- phyFSKFECScheme
  -> Signal dom Bit -- phyFSKFECEnabled
  -> Signal dom Bit -- phyFSKFECInterleavingRSC
  -> Signal dom Bit -- modulation
  -> Signal dom (Unsigned 10) -- phyFSKPreambleLength
  -> Signal dom Bit -- phyMRFSKSFD
  -> Signal dom Bit -- ready
  -> Signal dom Bit -- valid_i
  -> Signal dom (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)  -- ready_o, valid_o, data_o, last_o, shr_ready_i, splitter2_a_ready_i, scrambler_ready_i, test, test2
topEntity
  macFCSType
  phyFSKScramblePSDU
  phyFSKFECScheme
  phyFSKFECEnabled
  phyFSKFECInterleavingRSC
  modulation
  phyFSKPreambleLength
  phyMRFSKSFD
  ready_i
  valid_i = bundle (
    ready_o,
    valid_o,
    data_o,
    last_o,
    test0,
    test1,
    test2,
    test3)
  where
    -- MR-FSK modulator
    (ready_o, valid_o, data_o, last_o, psdu_ready_i, test0, test1, test2, test3) = unbundle $ mrFskModulator macFCSType phyFSKScramblePSDU phyFSKFECScheme phyFSKFECEnabled phyFSKFECInterleavingRSC modulation phyFSKPreambleLength phyMRFSKSFD phrFrameLength ready_i valid_i psdu_valid_o psdu_data_o psdu_last_o

    -- PSDU
    (psdu_valid_o, psdu_data_o, psdu_last_o, phrFrameLength) = unbundle $ psdu psdu_ready_i valid_i




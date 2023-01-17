module Tests.MR_FSK.PHR_PSDU.Top where

import Clash.Prelude

import Tests.MR_FSK.PHR_PSDU.PSDU
import SunPhy.MR_FSK.PHR
import SunPhy.Concat2


-- This is a test module to try the PHR, PSDU and Concat2 blocks

topEntity
  :: forall dom . (HiddenClockResetEnable dom)
  => Signal dom Bit -- macFCSType
  -> Signal dom Bit -- phyFSKScramblePSDU
  -> Signal dom Bit -- ready_i
  -> Signal dom Bit -- valid_i
  -> Signal dom (Bit, Bit, Bit) -- valid_o, data_o, last_o
topEntity macFCSType phyFSKScramblePSDU ready_i valid_i = bundle (valid_o, data_o, last_o)
  where
    phrModeSwitch = pure 0
    -- PHR
    (phr_valid_o, phr_data_o, phr_last_o) = unbundle $
      phr
        phrModeSwitch
        macFCSType
        phyFSKScramblePSDU
        phrFrameLength
        phr_ready_i
        valid_i
    -- PSDU
    (psdu_valid_o, psdu_data_o, psdu_last_o, phrFrameLength) = unbundle $ psdu psdu_ready_i valid_i

    -- Concat2
    (phr_ready_i, psdu_ready_i, valid_o, data_o, last_o) = unbundle $ concat2 phr_valid_o phr_data_o phr_last_o psdu_valid_o psdu_data_o psdu_last_o ready_i


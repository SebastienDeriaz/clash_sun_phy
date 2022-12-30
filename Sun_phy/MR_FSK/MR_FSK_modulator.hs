module Sun_phy.MR_FSK.MR_FSK_modulator where

import Clash.Prelude

import Sun_phy.MR_FSK.PHR (phr)
import Sun_phy.MR_FSK.PSDU (psdu)
import Sun_phy.Concat2 (concat2)
import Sun_phy.MR_FSK.FEC (fec)
import Sun_phy.MR_FSK.Interleaver (interleaver)
import Sun_phy.Splitter2 (splitter2)
import Sun_phy.Scrambler (scrambler)
import Sun_phy.MR_FSK.SHR (shr)
import Sun_phy.Concat3 (concat3)

nextReady :: Bit -> Bit -> Bit -> Bit
-- ready_o, last, ready_i
nextReady 1 1 _ = 0
nextReady 0 _ 1 = 1
nextReady x _ _ = x

splitterCounter :: Bit -> Unsigned 10
-- phyFSKFECEnabled
splitterCounter 0 = 16
splitterCounter 1 = 32

mrFskModulator
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Bit -- macFCSType
  -> Signal dom Bit -- phyFSKScramblePSDU
  -> Signal dom Bit -- phyFSKFECScheme
  -> Signal dom Bit -- phyFSKFECEnabled
  -> Signal dom Bit -- phyFSKFECInterleavingRSC
  -> Signal dom Bit -- modulation
  -> Signal dom (Unsigned 10) -- phyFSKPreambleLength
  -> Signal dom Bit -- phyMRFSKSFD
  -> Signal dom (Unsigned 11) -- phrFrameLength
  -> Signal dom Bit -- ready_i
  -> Signal dom Bit -- valid_i
  -> Signal dom Bit -- psdu_valid_i
  -> Signal dom Bit -- psdu_data_i
  -> Signal dom Bit -- psdu_last_i
  -> Signal dom (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit) -- ready_o, valid_o, data_o, last_o, psdu_ready_i, shr_ready_i, splitter2_a_ready_i, scrambler_ready_i, test, test 2
mrFskModulator
  -- Inputs
  macFCSType
  phyFSKScramblePSDU
  phyFSKFECScheme
  phyFSKFECEnabled
  phyFSKFECInterleavingRSC
  modulation
  phyFSKPreambleLength
  phyMRFSKSFD
  phrFrameLength
  ready_i
  valid_i
  psdu_valid_i
  psdu_data_i
  psdu_last_i = bundle(
    -- Outputs
    ready_o,
    valid_o,
    data_o,
    last_o,
    psdu_ready_i,
    test0,
    test1,
    test2,
    test3)
   where
    phrModeSwitch = pure 0

    test0 = splitter2_a_valid_o
    test1 = splitter2_a_last_o
    test2 = interleaver_valid_o
    test3 = interleaver_last_o

    -- PSDU is external
    -- PHR
    (phr_valid_o, phr_data_o, phr_last_o) = unbundle $ phr phrModeSwitch macFCSType phyFSKScramblePSDU phrFrameLength phr_ready_i valid_i
    -- Concat2
    (phr_ready_i, psdu_ready_i, concat2_valid_o, concat2_data_o, concat2_last_o) = unbundle $ concat2 phr_valid_o phr_data_o phr_last_o psdu_valid_i psdu_data_i psdu_last_i concat2_ready_i
    -- FEC
    fec_bypass = (1 - phyFSKFECEnabled)
    (concat2_ready_i, fec_valid_o, fec_data_o, fec_last_o) = unbundle $ fec fec_bypass phyFSKFECScheme concat2_valid_o concat2_data_o concat2_last_o fec_ready_i
    -- Interleaver
    interleaver_bypass = boolToBit <$> not <$> (phyFSKFECInterleavingRSC .==. pure 1 .&&. phyFSKFECEnabled .==. pure 1)
    (fec_ready_i, interleaver_data_o, interleaver_valid_o, interleaver_last_o) = unbundle $
      interleaver
        interleaver_bypass
        fec_valid_o
        fec_data_o
        fec_last_o
        interleaver_ready_i
    -- Splitter 2
    (interleaver_ready_i, splitter2_a_valid_o, splitter2_a_data_o, splitter2_a_last_o, splitter2_b_valid_o, splitter2_b_data_o, splitter2_b_last_o) = unbundle $ splitter2
      interleaver_valid_o
      interleaver_data_o
      interleaver_last_o
      splitter2_a_ready_i
      splitter2_b_ready_i
      (splitterCounter <$> phyFSKFECEnabled)
    -- Scrambler
    scrambler_bypass = (1-phyFSKScramblePSDU)
    (splitter2_b_ready_i, scrambler_valid_o, scrambler_data_o, scrambler_last_o) = unbundle $ scrambler scrambler_bypass scrambler_ready_i splitter2_b_valid_o splitter2_b_data_o splitter2_b_last_o (pure 511)
    -- SHR
    (shr_valid_o, shr_data_o, shr_last_o) = unbundle $ shr shr_ready_i modulation phyMRFSKSFD phyFSKFECEnabled phyFSKPreambleLength valid_i
    -- Concat3
    (shr_ready_i, splitter2_a_ready_i, scrambler_ready_i, valid_o, data_o, last_o) = unbundle $ concat3 shr_valid_o shr_data_o shr_last_o splitter2_a_valid_o splitter2_a_data_o splitter2_a_last_o scrambler_valid_o scrambler_data_o scrambler_last_o ready_i

    ready_o = register (0 :: Bit) (nextReady <$> ready_o <*> last_o <*> ready_i)
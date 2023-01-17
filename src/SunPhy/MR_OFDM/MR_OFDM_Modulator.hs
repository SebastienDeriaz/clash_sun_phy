module SunPhy.MR_OFDM.MR_OFDM_Modulator where

import Clash.Prelude

import SunPhy.MR_OFDM.STF
import SunPhy.MR_OFDM.PHR
import SunPhy.MR_OFDM.Encoder
import SunPhy.Concat4
import SunPhy.Settings


mrOfdmModulator
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Unsigned 3 -- ofdmOption
  -> Signal dom AxiBackward -- ready out
  -> Signal dom Bit -- valid_i
  -> Signal dom AxiForward -- PSDU
  -> Signal dom (AxiBackward, AxiForward, Bit, Bit, Bit, Bit, Bit) -- PSDU, output, psdu_ready_i
mrOfdmModulator
  -- Set scrambler seeds (See table 158 802.15.4g)
  
  -- Interleaver MCS can be different for PHR and Payload

  -- STF
  (phr_valid_o, phr_data_o, phr_last_o) = unbundle $ stf ofdmOption stf_ready_i stf_valid_i

  -- LTF
  (ltf_valid_o, ltf_data_o, ltf_last_o) = unbundle $ ltf ofdmOption ltf_ready_i ltf_valid_i

  -- PHR
  phrLength = pure 4
  (phr_valid_o, phr_data_o, phr_last_o) = unbundle $ phr rate frameLength phrLength phr_ready_i phr_valid_i

  -- PSDU
  -- 1) Scrambler
  (scrambler_ready_o, scrambler_valid_o, scrambler_data_o, scrambler_last_o) = unbundle $
    scrambler 0 scrambler_ready_i scrambler_valid_i scrambler_data_i scrambler_last_i scrambler_pn9_seed
  -- 2) Encoder
  (psdu_encoder_ready_o, psdu_encoder_valid_o, psdu_encoder_data_o, psdu_encoder_last_o) = unbundle $ encoder
    rate
    scrambler_valid_o
    scrambler_data_o
    scrambler_last_o
    psdu_encoder_ready_i
  -- 3) Interleaver
  (interleaver_ready_o, interleaver_valid_o, interleaver_data_o, interleaver_last_o) = unbundle $ interleaver
    
  


  -- Concatenate
  (a_ready, b_ready, c_ready, d_ready, concat4_valid, concat4_data, concat4_last) = Concat4
    a_valid_i
    a_data_i
    a_last_i
    b_valid_i
    b_data_i
    b_last_i
    c_valid_i
    c_data_i
    c_last_i
    d_valid_i
    d_data_i
    d_last_i
    concat4_ready_i
    


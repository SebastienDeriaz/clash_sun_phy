module SunPhy.MR_OFDM.MR_OFDM_Modulator where

import Clash.Prelude

import SunPhy.MR_OFDM.STF
import SunPhy.MR_OFDM.PHR
import SunPhy.MR_OFDM.Encoder
import SunPhy.MR_OFDM.Interleaver
import SunPhy.MR_OFDM.OFDM
import SunPhy.MR_OFDM.Constants
import SunPhy.Concat4


mrOfdmModulator
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom (Unsigned 3) -- ofdmOption
  -> Signal dom Bit -- ready_i
  -> Signal dom Bit -- valid_i
  -> Signal dom Bit -- psdu_valid_i
  -> Signal dom Bit -- psdu_data_i
  -> Signal dom Bit -- psdu_last_o
  -> Signal dom (Bit, Bit, Bit, Bit) -- PSDU, output, psdu_ready_o
mrOfdmModulator ofdmOption ready_i valid_i psdu_valid_i psdu_data_i psdu_last_i = psdu_ready_o valid_o data_o last_o
  where
    -- Set scrambler seeds (See table 158 802.15.4g)
    scrambler_pn9_seed = pure (0b1_1111_1111 :: BitVector 9)
  
    mcs = pure (1 :: Unsigned 3)
    phyOFDMInterleaving = pure (0 :: Bit)
    n_fft = pure (16 :: Unsigned 8)
    modulation = pure BPSK
    active_tones = pure (4 :: Unsigned 7)
    pilot_tones = pure (4 :: Unsigned 7)
    frequency_spreading = pure (0 :: Unsigned 3)
    -- Interleaver MCS can be different for PHR and Payload

    -- PSDU
    -- 1) Scrambler
    (psdu_ready_o, scrambler_valid_o, scrambler_data_o, scrambler_last_o) = unbundle $ scrambler
      0
      scrambler_ready_i
      psdu_valid_i
      psdu_data_i
      psdu_last_i
      scrambler_pn9_seed
    -- 2) Encoder
    (scrambler_ready_i, psdu_encoder_valid_o, psdu_encoder_data_o, psdu_encoder_last_o) = unbundle $ encoder
      rate
      scrambler_valid_o
      scrambler_data_o
      scrambler_last_o
      psdu_encoder_ready_i
    -- 3) Interleaver
    (psdu_encoder_ready_i, psdu_interleaver_valid_o, psdu_interleaver_data_o, psdu_interleaver_last_o) = unbundle $ interleaver
      0
      mcs
      ofdmOption
      phyOFDMInterleaving
      psdu_encoder_valid_o
      psdu_encoder_data_o
      psdu_encoder_last_o
      psdu_interleaver_ready_i
    -- 4) OFDM
    (psdu_pilot_ready, psdu_interleaver_ready_i, psdu_ofdm_valid_o, psdu_ofdm_data_o, psdu_ofdm_last_o) = unbundle $ ofdm
      n_fft
      modulation
      CP_QUARTER
      active_tones
      pilot_tones
      frequency_spreading
      psdu_pilot_valid_i
      psdu_pilot_relativeIndex
      psdu_pilot_value_i
      psdu_pilot_last_i
      psdu_interleaver_valid_o
      psdu_interleaver_data_o
      psdu_interleaver_last_o
      psdu_ofdm_ready_i

    -- PHR
    phrLength = pure 4
    -- 1) Generation
    (phr_valid_o, phr_data_o, phr_last_o) = unbundle $ phr rate frameLength phrLength phr_ready_i phr_valid_i
    -- 2) Encoder
    (phr_ready_i, phr_encoder_valid_o, phr_encoder_data_o, phr_encoder_last_o) = unbundle $ encoder
      rate
      phr_valid_o
      phr_data_o
      phr_last_o
      phr_encoder_ready_i
    -- 3) Interleaver
    (phr_interleaver_ready_o, phr_interleaver_valid_o, phr_interleaver_data_o, phr_interleaver_last_o) = unbundle $ interleaver
      lowest_mcs
      ofdmOption
      phyOFDMInterleaving
      phr_encoder_valid_o
      phr_encoder_data_o
      phr_encoder_last_o
      phr_interleaver_ready_i
    -- 4) OFDM
    (phr_pilot_ready, phr_data_ready, phr_ofdm_valid_o, phr_ofdm_data_o, phr_ofdm_last_o) = unbundle $ ofdm
      n_fft
      lowest_modulation
      CP_QUARTER
      active_tones
      pilot_tones
      frequency_spreading
      phr_pilot_valid_i
      phr_pilot_relativeIndex
      phr_pilot_value_i
      phr_pilot_last_i
      phr_interleaver_valid_o
      phr_interleaver_data_o
      phr_interleaver_last_o
      phr_ofdm_ready_i

    -- STF
    (stf_valid_o, stf_data_o, stf_last_o) = unbundle $ stf ofdmOption stf_ready_i stf_valid_i

    -- LTF
    (ltf_valid_o, ltf_data_o, ltf_last_o) = unbundle $ ltf ofdmOption ltf_ready_i ltf_valid_i

    -- Concatenate
    (stf_ready_i, ltf_ready_i, phr_ofdm_ready_i, psdu_ofdm_ready_i, valid_o, data_o, last_o) = Concat4
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
      ready_i
      


module SunPhy.MR_OFDM.MR_OFDM_Modulator where

import Clash.Prelude

import SunPhy.Concat4
import SunPhy.MR_OFDM.Constants
import SunPhy.MR_OFDM.Encoder
import SunPhy.MR_OFDM.Interleaver
import SunPhy.MR_OFDM.LTF
import SunPhy.MR_OFDM.OFDM
import SunPhy.MR_OFDM.PHR
import SunPhy.MR_OFDM.STF
import SunPhy.Scrambler

mrOfdmModulator
    :: forall dom
     . HiddenClockResetEnable dom
    => Signal dom OFDM_Option
    -> Signal dom MCS
    -> Signal dom Bit -- phyOFDMInterleaving
    -> Signal dom Bit -- ready_i
    -> Signal dom Bit -- start_i
    -> Signal dom Bit -- psdu_valid_i
    -> Signal dom Bit -- psdu_data_i
    -> Signal dom Bit -- psdu_last_o
    -> Signal dom (Unsigned 11) -- psdu_length (octets)
    -> Signal dom (Bit, Bit, IQ, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit) -- PSDU, output, psdu_ready_o
mrOfdmModulator
    -- Inputs
    ofdmOption
    mcs
    phyOFDMInterleaving
    ready_i
    start_i
    psdu_valid_i
    psdu_data_i
    psdu_last_i
    psdu_length =
        -- Outputs
        bundle
            ( psdu_ready_o
            , valid_o
            , data_o
            , last_o
            , phr_encoder_ready_i
            , phr_encoder_valid_o
            , phr_interleaver_ready_i
            , phr_interleaver_valid_o
            , phr_ofdm_ready_i
            , phr_ofdm_valid_o
            , phr_interleaver_ready_i * phr_interleaver_valid_o
            )
        where
            -- Set scrambler seeds (See table 158 802.15.4g)
            scrambler_pn9_seed = pure (0b1_1111_1111 :: BitVector 9)

            phyOFDMInterleaving = pure (0 :: Bit)
            scrambler_seed = pure 0

            lowest_mcs = lowestMCS <$> ofdmOption
            lowest_modulation = mcsModulation <$> lowest_mcs
            -- Interleaver MCS can be different for PHR and Payload

            -- PSDU
            -- 1) Scrambler
            (psdu_ready_o, scrambler_valid_o, scrambler_data_o, scrambler_last_o) =
                unbundle $
                    scrambler
                        (pure 0)
                        scrambler_ready_i
                        psdu_valid_i
                        psdu_data_i
                        psdu_last_i
                        scrambler_pn9_seed
            -- 2) Encoder
            (scrambler_ready_i, psdu_encoder_valid_o, psdu_encoder_data_o, psdu_encoder_last_o) =
                unbundle $
                    encoder
                        (rate <$> mcs)
                        scrambler_valid_o
                        scrambler_data_o
                        scrambler_last_o
                        psdu_encoder_ready_i
            -- 3) Interleaver
            (psdu_encoder_ready_i, psdu_interleaver_valid_o, psdu_interleaver_data_o, psdu_interleaver_last_o) =
                unbundle $
                    interleaver
                        0
                        mcs
                        ofdmOption
                        phyOFDMInterleaving
                        psdu_encoder_valid_o
                        psdu_encoder_data_o
                        psdu_encoder_last_o
                        psdu_interleaver_ready_i
            -- 4) OFDM
            (psdu_interleaver_ready_i, psdu_ofdm_valid_o, psdu_ofdm_data_o, psdu_ofdm_last_o, _, _) =
                unbundle $
                    ofdm
                        ofdmOption
                        mcs
                        (pure 1)
                        psdu_interleaver_valid_o
                        psdu_interleaver_data_o
                        psdu_interleaver_last_o
                        phr_pilotset
                        phr_ofdm_last_o
                        pn9_seed
                        phr_ofdm_last_o
                        psdu_ofdm_ready_i

            -- PHR
            phrLength = (resize <$> (n_cbps <$> ofdmOption <*> lowest_mcs <*> phyOFDMInterleaving)) * (resize <$> (phrNSymbols <$> ofdmOption <*> phyOFDMInterleaving))
            -- 1) Generation
            (phr_valid_o, phr_data_o, phr_last_o, counter) = unbundle $ phr mcs psdu_length scrambler_seed phrLength phr_ready_i start_i
            -- 2) Encoder
            (phr_ready_i, phr_encoder_valid_o, phr_encoder_data_o, phr_encoder_last_o) =
                unbundle $
                    encoder
                        (rate <$> lowest_mcs)
                        phr_valid_o
                        phr_data_o
                        phr_last_o
                        phr_encoder_ready_i
            -- 3) Interleaver
            (phr_encoder_ready_i, phr_interleaver_valid_o, phr_interleaver_data_o, phr_interleaver_last_o) =
                unbundle $
                    interleaver
                        (pure 0)
                        lowest_mcs
                        ofdmOption
                        phyOFDMInterleaving
                        phr_encoder_valid_o
                        phr_encoder_data_o
                        phr_encoder_last_o
                        phr_interleaver_ready_i
            -- 4) OFDM
            (phr_interleaver_ready_i, phr_ofdm_valid_o, phr_ofdm_data_o, phr_ofdm_last_o, phr_pilotset, pn9_seed) =
                unbundle $
                    ofdm
                        ofdmOption
                        mcs
                        (pure 1)
                        phr_interleaver_valid_o
                        phr_interleaver_data_o
                        phr_interleaver_last_o
                        (pure 0)
                        (pure 0)
                        (pure 0)
                        (pure 0)
                        phr_ofdm_ready_i

            -- STF
            (stf_valid_o, stf_data_o, stf_last_o) = unbundle $ stf ofdmOption stf_ready_i start_i

            -- LTF
            (ltf_valid_o, ltf_data_o, ltf_last_o) = unbundle $ ltf ofdmOption ltf_ready_i start_i

            -- Concatenate
            (stf_ready_i, ltf_ready_i, phr_ofdm_ready_i, psdu_ofdm_ready_i, valid_o, data_o, last_o, state_num) =
                unbundle $
                    concat4
                        stf_valid_o
                        stf_data_o
                        stf_last_o
                        ltf_valid_o
                        ltf_data_o
                        ltf_last_o
                        phr_ofdm_valid_o
                        phr_ofdm_data_o
                        phr_ofdm_last_o
                        psdu_ofdm_valid_o
                        psdu_ofdm_data_o
                        psdu_ofdm_last_o
                        ready_i

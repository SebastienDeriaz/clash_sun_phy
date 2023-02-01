{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MrOfdmModulatorTester where

import Clash.Prelude

import Data.Functor ((<&>))
import SunPhy.AXI
import SunPhy.Concat4
import SunPhy.MR_OFDM.Constants
import SunPhy.MR_OFDM.MR_OFDM_Modulator
import SunPhy.Parallelizer
import SunPhy.Serializer

data MrOfdmModulatorTesterInput n = MrOfdmModulatorTesterInput
    { ofdmOption :: OFDM_Option
    , mcs :: MCS
    , phyOFDMInterleaving :: Bit
    , scramblerSeed :: Unsigned 2
    , _data :: Vec n Bit
    , start :: Bit
    , ready :: Bit
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data MrOfdmModulatorTesterOutput m = MrOfdmModulatorTesterOutput
    { _data :: Vec m IQ
    , valid :: Bit
    , ready :: Bit
    , -- Debug
      -- ready signals
      stfReady :: Bit
    , ltfReady :: Bit
    , phrReady :: Bit
    , psduReady :: Bit
    , -- streams (PHR)
      phr_ready_i :: Bit
    , phr_valid_o :: Bit
    , phr_last_o :: Bit
    , phr_encoder_ready_i :: Bit
    , phr_encoder_valid_o :: Bit
    , phr_encoder_last_o :: Bit
    , phr_interleaver_ready_i :: Bit
    , phr_interleaver_valid_o :: Bit
    , phr_interleaver_last_o :: Bit
    , phr_ofdm_ready_i :: Bit
    , phr_ofdm_valid_o :: Bit
    , phr_ofdm_last_o :: Bit
    , -- streams (PSDU)
      psdu_ready_i
    , psdu_valid_o
    , psdu_last_o
    , psdu_padder_ready_i
    , psdu_padder_valid_o
    , psdu_padder_last_o
    , psdu_scrambler_ready_i
    , psdu_scrambler_valid_o
    , psdu_scrambler_last_o
    , psdu_encoder_ready_i
    , psdu_encoder_valid_o
    , psdu_encoder_last_o
    , psdu_puncturer_ready_i
    , psdu_puncturer_valid_o
    , psdu_puncturer_last_o
    , psdu_interleaver_ready_i :: Bit
    , psdu_interleaver_valid_o :: Bit
    , psdu_interleaver_last_o :: Bit
    , psdu_ofdm_ready_i :: Bit
    , psdu_ofdm_valid_o :: Bit
    , psdu_ofdm_last_o :: Bit
    , -- other
      last :: Bit
    , phrInterleaverMasterCounter :: Unsigned 16
    , phrInterleaverSlaveCounter :: Unsigned 16
    , phrEncoderSlaveCounter :: Unsigned 10
    , phrEncoderMasterCounter :: Unsigned 10
    , psduInterleaverMasterCounter :: Unsigned 16
    , psduInterleaverSlaveCounter :: Unsigned 16
    , psduPadderCounter :: Unsigned 9
    , psduPadderState :: Unsigned 3
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

mrOfdmModulatorTester
    :: forall n m dom
     . HiddenClockResetEnable dom
    => KnownNat n
    => KnownNat m
    => Signal dom (MrOfdmModulatorTesterInput n)
    -> Signal dom (MrOfdmModulatorTesterOutput m)
mrOfdmModulatorTester input = do
    ready <- serializerOutput <&> (.axiInputFeedback) <&> (.ready)
    valid <- parallelizerOutput <&> (.valid)
    _data <- parallelizerOutput <&> (._data)
    -- Ready signals
    stfReady <- mrOfdmModulatorOutput <&> (.stfReady)
    ltfReady <- mrOfdmModulatorOutput <&> (.ltfReady)
    phrReady <- mrOfdmModulatorOutput <&> (.phrReady)
    psduReady <- mrOfdmModulatorOutput <&> (.psduReady)
    -- streams (PHR)
    phr_ready_i <- mrOfdmModulatorOutput <&> (.phr_ready_i)
    phr_valid_o <- mrOfdmModulatorOutput <&> (.phr_valid_o)
    phr_last_o <- mrOfdmModulatorOutput <&> (.phr_last_o)
    phr_encoder_ready_i <- mrOfdmModulatorOutput <&> (.phr_encoder_ready_i)
    phr_encoder_valid_o <- mrOfdmModulatorOutput <&> (.phr_encoder_valid_o)
    phr_encoder_last_o <- mrOfdmModulatorOutput <&> (.phr_encoder_last_o)
    phr_interleaver_ready_i <- mrOfdmModulatorOutput <&> (.phr_interleaver_ready_i)
    phr_interleaver_valid_o <- mrOfdmModulatorOutput <&> (.phr_interleaver_valid_o)
    phr_interleaver_last_o <- mrOfdmModulatorOutput <&> (.phr_interleaver_last_o)
    phr_ofdm_ready_i <- mrOfdmModulatorOutput <&> (.phr_ofdm_ready_i)
    phr_ofdm_valid_o <- mrOfdmModulatorOutput <&> (.phr_ofdm_valid_o)
    phr_ofdm_last_o <- mrOfdmModulatorOutput <&> (.phr_ofdm_last_o)
    -- streams (PSDU)
    psdu_ready_i <- mrOfdmModulatorOutput <&> (.psdu_ready_i)
    psdu_valid_o <- mrOfdmModulatorOutput <&> (.psdu_valid_o)
    psdu_last_o <- mrOfdmModulatorOutput <&> (.psdu_last_o)
    psdu_padder_ready_i <- mrOfdmModulatorOutput <&> (.psdu_padder_ready_i)
    psdu_padder_valid_o <- mrOfdmModulatorOutput <&> (.psdu_padder_valid_o)
    psdu_padder_last_o <- mrOfdmModulatorOutput <&> (.psdu_padder_last_o)
    psdu_scrambler_ready_i <- mrOfdmModulatorOutput <&> (.psdu_scrambler_ready_i)
    psdu_scrambler_valid_o <- mrOfdmModulatorOutput <&> (.psdu_scrambler_valid_o)
    psdu_scrambler_last_o <- mrOfdmModulatorOutput <&> (.psdu_scrambler_last_o)
    psdu_encoder_ready_i <- mrOfdmModulatorOutput <&> (.psdu_encoder_ready_i)
    psdu_encoder_valid_o <- mrOfdmModulatorOutput <&> (.psdu_encoder_valid_o)
    psdu_encoder_last_o <- mrOfdmModulatorOutput <&> (.psdu_encoder_last_o)
    psdu_puncturer_ready_i <- mrOfdmModulatorOutput <&> (.psdu_puncturer_ready_i)
    psdu_puncturer_valid_o <- mrOfdmModulatorOutput <&> (.psdu_puncturer_valid_o)
    psdu_puncturer_last_o <- mrOfdmModulatorOutput <&> (.psdu_puncturer_last_o)
    psdu_interleaver_ready_i <- mrOfdmModulatorOutput <&> (.psdu_interleaver_ready_i)
    psdu_interleaver_valid_o <- mrOfdmModulatorOutput <&> (.psdu_interleaver_valid_o)
    psdu_interleaver_last_o <- mrOfdmModulatorOutput <&> (.psdu_interleaver_last_o)
    psdu_ofdm_ready_i <- mrOfdmModulatorOutput <&> (.psdu_ofdm_ready_i)
    psdu_ofdm_valid_o <- mrOfdmModulatorOutput <&> (.psdu_ofdm_valid_o)
    psdu_ofdm_last_o <- mrOfdmModulatorOutput <&> (.psdu_ofdm_last_o)
    -- other
    last <- mrOfdmModulatorOutput <&> (.axiOutput) <&> (.last)
    phrInterleaverMasterCounter <- mrOfdmModulatorOutput <&> (.phrInterleaverMasterCounter)
    phrInterleaverSlaveCounter <- mrOfdmModulatorOutput <&> (.phrInterleaverSlaveCounter)
    phrEncoderSlaveCounter <- mrOfdmModulatorOutput <&> (.phrEncoderSlaveCounter)
    phrEncoderMasterCounter <- mrOfdmModulatorOutput <&> (.phrEncoderMasterCounter)
    psduInterleaverMasterCounter <- mrOfdmModulatorOutput <&> (.psduInterleaverMasterCounter)
    psduInterleaverSlaveCounter <- mrOfdmModulatorOutput <&> (.psduInterleaverSlaveCounter)
    psduPadderCounter <- mrOfdmModulatorOutput <&> (.psduPadderCounter)
    psduPadderState <- mrOfdmModulatorOutput <&> (.psduPadderState)
    pure $ MrOfdmModulatorTesterOutput {..}
    where
        serializerInput :: Signal dom (SerializerInput n Bit)
        serializerInput =
            bundle (input, mrOfdmModulatorOutput)
                <&> \(input, mrOfdmModulatorOutput) ->
                    SerializerInput
                        { dataVec = input._data
                        , start = input.start
                        , axiOutputFeedback = mrOfdmModulatorOutput.psduAxiInputFeedback
                        }

        serializerOutput :: Signal dom (SerializerOutput Bit)
        serializerOutput = serializer serializerInput

        mrOfdmModulatorInput :: Signal dom MrOfdmModulatorInput
        mrOfdmModulatorInput =
            bundle (input, serializerOutput, parallelizerOutput)
                <&> \(input, serializerOutput, parallelizerOutput) ->
                    MrOfdmModulatorInput
                        { ofdmOption = input.ofdmOption
                        , mcs = input.mcs
                        , phyOFDMInterleaving = input.phyOFDMInterleaving
                        , scramblerSeed = input.scramblerSeed
                        , start = input.start
                        , psduLength = fromIntegral $ length input._data `div` 8
                        , psduAxiInput = serializerOutput.axiOutput
                        , axiOutputFeedback = parallelizerOutput.axiInputFeedback
                        }
        mrOfdmModulatorOutput :: Signal dom MrOfdmModulatorOutput
        mrOfdmModulatorOutput = mrOfdmModulator mrOfdmModulatorInput

        parallelizerInput :: Signal dom (ParallelizerInput IQ)
        parallelizerInput =
            bundle (input, mrOfdmModulatorOutput)
                <&> \(input, mrOfdmModulatorOutput) ->
                    ParallelizerInput
                        { axiInput = mrOfdmModulatorOutput.axiOutput
                        , axiOutputFeedback =
                            AxiBackward
                                { ready = input.ready
                                }
                        }
        parallelizerOutput :: Signal dom (ParallelizerOutput m IQ)
        parallelizerOutput = parallelizer parallelizerInput

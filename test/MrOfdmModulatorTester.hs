{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MrOfdmModulatorTester where

import Clash.Prelude

import Data.Functor ((<&>))
import GHC.RTS.Flags (DebugFlags (DebugFlags))
import SunPhy.MR_OFDM.Constants
import SunPhy.MR_OFDM.MR_OFDM_Modulator
import SunPhy.Parallelizer
import SunPhy.Serializer

data MrOfdmModulatorTesterInput n = MrOfdmModulatorTesterInput
    { ofdmOption :: OFDM_Option
    , mcs :: MCS
    , phyOFDMInterleaving :: Bit
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
      psdu_ready :: Bit
    , psdu_valid :: Bit
    , ofdm_valid :: Bit
    , ofdm_ready :: Bit
    , phr_encoder_ready_i :: Bit
    , phr_encoder_valid_o :: Bit
    , phr_interleaver_ready_i :: Bit
    , phr_interleaver_valid_o :: Bit
    , phr_ofdm_ready_i :: Bit
    , phr_ofdm_valid_o :: Bit
    , phr_ofdm_write :: Bit
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
    ready <- serializerOutput <&> (.ready)
    valid <- parallelizerOutput <&> (.valid)
    _data <- parallelizerOutput <&> (._data)
    psdu_ready <- serializerInput <&> (.ready)
    psdu_valid <- serializerOutput <&> (.valid)
    ofdm_valid <- valid_o
    ofdm_ready <- parallelizerOutput <&> (.ready)
    phr_encoder_ready_i <- phr_encoder_ready_i
    phr_encoder_valid_o <- phr_encoder_valid_o
    phr_interleaver_ready_i <- phr_interleaver_ready_i
    phr_interleaver_valid_o <- phr_interleaver_valid_o
    phr_ofdm_ready_i <- phr_ofdm_ready_i
    phr_ofdm_valid_o <- phr_ofdm_valid_o
    phr_ofdm_write <- phr_ofdm_write
    pure $ MrOfdmModulatorTesterOutput {..}
    where
        serializerInput :: Signal dom (SerializerInput n Bit)
        serializerInput =
            bundle (input, psdu_ready_o)
                <&> \(input, psdu_ready_o) ->
                    SerializerInput
                        { _data = input._data
                        , start = input.start
                        , ready = psdu_ready_o
                        }

        serializerOutput :: Signal dom (SerializerOutput Bit)
        serializerOutput = serializer serializerInput

        (psdu_ready_o
            , valid_o
            , data_o
            , last_o
            , phr_encoder_ready_i
            , phr_encoder_valid_o
            , phr_interleaver_ready_i
            , phr_interleaver_valid_o
            , phr_ofdm_ready_i
            , phr_ofdm_valid_o
            , phr_ofdm_write) =
                unbundle $
                    mrOfdmModulator
                        (ofdmOption <$> input)
                        (mcs <$> input)
                        (phyOFDMInterleaving <$> input)
                        (parallelizerOutput <&> (.ready))
                        (input <&> (.start))
                        (serializerOutput <&> (.valid))
                        (serializerOutput <&> (._data))
                        (serializerOutput <&> (.last))
                        (fromIntegral <$> ((div 8) <$> (length <$> (input <&> (._data)))))

        parallelizerInput :: Signal dom (ParallelizerInput IQ)
        parallelizerInput =
            bundle (input, valid_o, data_o, last_o)
                <&> \(input, valid_o, data_o, last_o) ->
                    ParallelizerInput
                        { ready = input.ready
                        , valid = valid_o
                        , _data = data_o
                        , last = last_o
                        }
        parallelizerOutput :: Signal dom (ParallelizerOutput m IQ)
        parallelizerOutput = parallelizer parallelizerInput

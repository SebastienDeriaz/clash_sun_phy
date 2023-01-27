{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MrOfdmModulatorTester where

import Clash.Prelude

-- (serializer, SerializerInput, SerializerOutput)

import Data.Functor ((<&>))
import SunPhy.MR_OFDM.Constants
import SunPhy.MR_OFDM.MR_OFDM_Modulator
import SunPhy.Serializer

data MrOfdmModulatorTesterInput n a = MrOfdmModulatorTesterInput
    { ofdmOption :: OFDM_Option
    , mcs :: MCS
    , phyOFDMInterleaving :: Bit
    , data_i :: Vec n Bit
    , start_i :: Bit
    , ready_i :: Bit
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data MrOfdmModulatorTesterOutput a = MrOfdmModulatorTesterOutput
    { data_o :: IQ
    , ready_o :: Bit
    , valid_o :: Bit
    , last_o :: Bit
    }
    deriving stock (Generic, Show)
    deriving anyclass (NFDataX)

mrOfdmModulatorTester
    :: forall a n dom
     . HiddenClockResetEnable dom
    => KnownNat n
    => NFDataX a
    => Signal dom (MrOfdmModulatorTesterInput n a)
    -> Signal dom (MrOfdmModulatorTesterOutput a)
mrOfdmModulatorTester input = do
    ready_o <- outputReady <$> serializerOutput
    valid_o <- valid_o
    data_o <- data_o
    last_o <- last_o
    pure $ MrOfdmModulatorTesterOutput {..}
    where
        serializerInput :: Signal dom (SerializerInput n Bit)
        serializerInput =
            bundle (input, psdu_ready_o)
                <&> \(input, psdu_ready_o) ->
                    SerializerInput
                        { inputData = data_i input
                        , inputStart = start_i input
                        , inputReady = psdu_ready_o
                        }

        serializerOutput :: Signal dom (SerializerOutput Bit)
        serializerOutput = serializer serializerInput -- isn't this neat?
        -- this is yes
        psdu_length :: Signal dom (Unsigned 11)
        psdu_length = fromIntegral <$> ((div 8) <$> (length <$> (data_i <$> input)))

        (psdu_ready_o, valid_o, data_o, last_o) =
            unbundle $
                mrOfdmModulator
                    (ofdmOption <$> input)
                    (mcs <$> input)
                    (phyOFDMInterleaving <$> input)
                    (ready_i <$> input)
                    (start_i <$> input)
                    (outputValid <$> serializerOutput) -- psdu_valid_o
                    (outputData <$> serializerOutput) -- psdu_data_o
                    (outputLast <$> serializerOutput) -- psdu_last_o
                    psdu_length

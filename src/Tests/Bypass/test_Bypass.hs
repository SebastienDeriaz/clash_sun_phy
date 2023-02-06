{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tests.Bypass.TestBypass where

import Clash.Prelude

import SunPhy.Bypass
import SunPhy.AXI
import Data.Functor ((<&>))

testBypass
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Bit -- valid_i
  -> Signal dom Bit -- data_i
  -> Signal dom Bit -- last_i
  -> Signal dom Bit -- ready_i
  -> Signal dom (Bit, Bit, Bit, Bit)
testBypass valid_i data_i last_i ready_i = bundle (
        bypassOutput <&> (.axiOutput) <&> (.valid),
        bypassOutput <&> (.axiOutput) <&> (._data),
        bypassOutput <&> (.axiOutput) <&> (.last),
        bypassOutput <&> (.axiInputFeedback) <&> (.ready)
    )
    where
        bypassInput = do
            axiInput <- do
                valid <- valid_i
                _data <- data_i
                last <- last_i
                pure AxiForward {..}
            axiOutputFeedback <- do
                ready <- ready_i
                pure AxiBackward {..}
            pure BypassInput {..}
        bypassOutput = bypass bypassInput
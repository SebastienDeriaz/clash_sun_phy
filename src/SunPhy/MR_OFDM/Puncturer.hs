{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}

-- The puncturer takes a 6 bit bitstream and removes some bits as such :
-- a0,b0,a1,b1,a2,b2 -> a0,b0,a1,b2
-- If disabled, the data is simply bypassed
--
--                  ┏━━━━━━━━━━━━━━━┓
--                  ┃   Puncturer   ┃
--          enable  ┣>             <┨  ready_i
--         ready_o <┨               ┠> valid_o
--         valid_i  ┠>              ┣> data_o
--          data_i  ┠>              ┠> last_o
--          last_i  ┠>              ┃
--                  ┗━━━━━━━━━━━━━━━┛

module SunPhy.MR_OFDM.Puncturer where

import Clash.Prelude
import SunPhy.Bypass (bypass, BypassInput(..), BypassOutput(..))
import Data.Functor ((<&>))
import SunPhy.AXI


data PuncturerInput = PuncturerInput
    { enable :: Bit
    , axiInput :: AxiForward Bit
    , axiOutputFeedback :: AxiBackward
    , reset :: Bit
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data PuncturerOutput = PuncturerOutput
    { axiInputFeedback :: AxiBackward
    , axiOutput :: AxiForward Bit
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

puncturer
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom PuncturerInput
  -> Signal dom PuncturerOutput
puncturer input = do
    -- Input feedback (to master)
    axiInputFeedback <- do
      ready <- readyOut
      pure AxiBackward {..}
    -- Output (to slave)
    axiOutput <- do
      last <- bypassOutput <&> (.axiOutput) <&> (.last)
      valid <- bypassOutput <&> (.axiOutput) <&> (.valid)
      _data <- bypassOutput <&> (.axiOutput) <&> (._data)
      pure AxiForward {..}
    pure PuncturerOutput {..}
  where
    readyOut = mux
      keep
      (bypassOutput <&> (.axiInputFeedback) <&> (.ready))
      1
    slaveWrite = readyOut .==. 1 .&&. (input <&> (.axiInput) <&> (.valid)) .==. 1

    nextBitCounter :: Bit -> Bool -> Unsigned 3 -> Unsigned 3
    -- Reseting
    nextBitCounter 1 _ _ = 0
    -- Slave write
    nextBitCounter 0 True x = x + 1
    -- Idle
    nextBitCounter 0 False x = x

    bitCounter = register (0 :: Unsigned 3) $ nextBitCounter
        <$> (input <&> (.reset))
        <*> slaveWrite
        <*> bitCounter

    -- Decides if the data must be kept or not
    -- Data B1 and A2 is removed when the puncturer is enabled
    keep = bitCounter .==. 3 .||. bitCounter .==. 4 .||. (input <&> (.enable)) .==. 0

    bypassInput = do
      axiInput <- do
        valid <- mux keep (input <&> (.axiInput) <&> (.valid)) 0
        _data <- input <&> (.axiInput) <&> (._data)
        last <- input <&> (.axiInput) <&> (.last)
        pure AxiForward {..}
      axiOutputFeedback <- do
        ready <- input <&> (.axiOutputFeedback) <&> (.ready)
        pure AxiBackward {..}
      pure BypassInput {..}

    bypassOutput = bypass bypassInput
        
        


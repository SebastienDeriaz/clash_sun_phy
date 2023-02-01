module SunPhy.MR_OFDM.LTF where

import Clash.Prelude
import Data.Functor ((<&>))
import SunPhy.MR_OFDM.Constants
import SunPhy.MR_OFDM.LTF_constants
import SunPhy.AXI

-- The LTF symbols are repeated twice and a CP of half a symbol is included in the beginning
-- So if a symbol is composed as such
-- AB
-- Then the message is
-- BABAB

data LTFInput = LTFInput
    { ofdmOption :: OFDM_Option
    , axiOutputFeedback :: AxiBackward
    , start :: Bit
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data LTFOutput = LTFOutput
    { axiOutput :: AxiForward IQ
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data State = Idle
           | Running
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX


ltf
  :: forall dom . (HiddenClockResetEnable dom)
  => Signal dom LTFInput
  -> Signal dom LTFOutput
ltf input = do
  axiOutput <- do
    valid <- valid_o
    _data <- dataOut <$> ofdmOption <*> bitCounter
    last <- boolToBit <$> (bitCounterEnd .==. 1 .&&. symbolCounterEnd .==. 1)
    pure AxiForward {..}
  pure LTFOutput {..}
  where
    slaveWrite = valid_o * (input <&> (.axiOutputFeedback) <&> (.ready))
    start_i = input <&> (.start)
    ofdmOption = input <&> (.ofdmOption)

    nextState :: State -> Bit -> Bit -> Bit -> Bit -> State
    nextState Idle    1 _ _ _ = Running
    nextState Running _ 1 1 1 = Idle
    nextState x       _ _ _ _ = x

    state = register (Idle) $ nextState <$> state <*> start_i <*> slaveWrite <*> bitCounterEnd <*> symbolCounterEnd

    nextBitCounter :: State -> Unsigned 3 -> Bit -> Bit -> Bit -> Unsigned 8 -> Unsigned 8
    --             ┌state  ┌ofdmOption
    --             │       │ ┌start_i
    --             │       │ │ ┌slaveWrite
    --             │       │ │ │ ┌bitCounterEnd
    --             │       │ │ │ │ ┌bitCounter
    --             │       │ │ │ │ │
    -- Idle        │       │ │ │ │ │
    nextBitCounter Idle    o _ _ _ _ = (ltfLength o) `div` 2
    nextBitCounter Running _ _ 1 1 _ = 0
    nextBitCounter Running _ _ 1 0 x = x + 1
    nextBitCounter _       _ _ _ _ x = x

    bitCounter = register (0 :: Unsigned 8) (nextBitCounter <$> state <*> ofdmOption <*> start_i <*> slaveWrite <*> bitCounterEnd <*> bitCounter)
    bitCounterEnd = boolToBit <$> (bitCounter .==. ((ltfLength <$> ofdmOption) - 1))

    ltfLength :: Unsigned 3 -> Unsigned 8
    ltfLength 1 = fromIntegral $ length ltf_1
    ltfLength 2 = fromIntegral $ length ltf_2
    ltfLength 3 = fromIntegral $ length ltf_3
    ltfLength 4 = fromIntegral $ length ltf_4
    ltfLength _ = 0

    nextSymbolCounter :: State -> Bit -> Bit -> Unsigned 2 -> Unsigned 2
    nextSymbolCounter Idle    _ _ _ = 0
    nextSymbolCounter Running 1 1 x = x + 1
    nextSymbolCounter _       _ _ x = x
    symbolCounter = register (0 :: Unsigned 2) (nextSymbolCounter <$> state <*> bitCounterEnd <*> slaveWrite <*> symbolCounter)
    symbolCounterEnd = boolToBit <$> (symbolCounter .==. 2)

    dataOut :: Unsigned 3 -> Unsigned 8 -> Subcarrier
    dataOut 1 i = ltf_1 !! i
    dataOut 2 i = ltf_2 !! i
    dataOut 3 i = ltf_3 !! i
    dataOut 4 i = ltf_4 !! i

    valid_o = boolToBit <$> (state .==. pure Running)
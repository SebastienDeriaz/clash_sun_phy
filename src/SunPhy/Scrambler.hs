module SunPhy.Scrambler where

import SunPhy.PN9 (pn9)
import SunPhy.AXI
import Clash.Prelude
import Data.Functor ((<&>))


data ScramblerInput = ScramblerInput
    { axiInput :: AxiForward Bit
    , axiOutputFeedback :: AxiBackward
    , pn9Seed :: BitVector 9
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data ScramblerOutput = ScramblerOutput
    { axiInputFeedback :: AxiBackward
    , axiOutput :: AxiForward Bit
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

nextLastEnable :: Bit -> Unsigned 2 -> Bit -> Bit
-- lastEnable, buffer, last_i
nextLastEnable _ 0 _ = 0
nextLastEnable _ _ 1 = 1
nextLastEnable x _ _ = x

nextBuffer :: Bit -> Bit -> Unsigned 2 -> Unsigned 2
-- slaveWrite, masterWrite, buffer
-- No change
nextBuffer 0 0 x = x
nextBuffer 1 1 x = x
-- Increment
nextBuffer 0 1 2 = 2
nextBuffer 0 1 x = x + 1
-- Decrement 
nextBuffer 1 0 0 = 0
nextBuffer 1 0 x = x - 1

scrambler
    :: forall dom . HiddenClockResetEnable dom
    => Signal dom ScramblerInput
    -> Signal dom ScramblerOutput
scrambler input = do
  -- Input feedback
  axiInputFeedback <- do
    ready <- ready_o
    pure AxiBackward {..}
  -- Output
  axiOutput <- do
    valid <- valid_o
    _data <- data_o
    last <- last_o
    pure AxiForward {..}
    
  pure ScramblerOutput {..}

  where
    ready_i = input <&> (.axiOutputFeedback) <&> (.ready)


    slaveWrite = ready_i * valid_o
    masterWrite = ready_o * (input <&> (.axiInput) <&> (.valid))

    buffer = register (0 :: Unsigned 2) nextBuffer'
    nextBuffer' = nextBuffer <$> slaveWrite <*> masterWrite <*> buffer
    a_nb = buffer ./=. pure 2

    lastEnable = register (0 :: Bit) $ nextLastEnable
      <$> lastEnable
      <*> buffer
      <*> (input <&> (.axiInput) <&> (.last))

    pn9_next = masterWrite
    pn9_reset = last_o

    pn9_value :: Signal dom Bit
    (pn9_value, _) = unbundle $ pn9
      (input <&> (.pn9Seed))
      pn9_next
      pn9_reset


    scrambledInput = xor <$> pn9_value <*> (input <&> (.axiInput) <&> (._data))

    a = register (0 :: Bit) nextA
    nextA = mux (bitToBool <$> masterWrite) scrambledInput a

    b = register (0 :: Bit) nextB
    nextB = mux (bitToBool <$> masterWrite) a b

    -- Outputs
    ready_o = register (0 :: Bit) ready_i
    valid_o = boolToBit <$> (buffer .>. pure 0)
    data_o = mux a_nb a b
    last_o = boolToBit <$> (buffer .==. pure 1 .&&. nextBuffer' .==. pure 0 .&&. lastEnable .==. pure 1)

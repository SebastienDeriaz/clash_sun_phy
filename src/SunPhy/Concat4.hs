{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module SunPhy.Concat4 where

import Clash.Prelude
import SunPhy.MR_OFDM.Constants
import SunPhy.AXI
import Data.Functor ((<&>))

data Concat4Input = Concat4Input
    { axiInputA :: AxiForward IQ
    , axiInputB :: AxiForward IQ
    , axiInputC :: AxiForward IQ
    , axiInputD :: AxiForward IQ
    , axiOutputFeedback :: AxiBackward
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data Concat4Output = Concat4Output
    { axiInputFeedbackA :: AxiBackward
    , axiInputFeedbackB :: AxiBackward
    , axiInputFeedbackC :: AxiBackward
    , axiInputFeedbackD :: AxiBackward
    , axiOutput :: AxiForward IQ
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)


-- State machine
data State = A
           | B
           | C
           | D
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX

nextState :: State -> Bit -> Bit -> Bit -> Bit -> Bit -> State
-- state, slaveWrite, a_last_i, b_last_i, c_last_i
nextState A 1 1 _ _ _ = B -- to B if the slave did write and a_last_i is 1
nextState B 1 _ 1 _ _ = C -- to C if the slave did write and b_last_i is 1
nextState C 1 _ _ 1 _ = D -- to D if the slave did write and c_last_i is 1
nextState D 1 _ _ _ 1 = A -- to A if the slave did write and d_last_i is 1
nextState x _ _ _ _ _ = x


mux4 :: NFDataX a => State -> a -> a -> a -> a -> a
mux4 state a b c d = case state of
    A -> a
    B -> b
    C -> c
    D -> d

concat4
    :: forall dom . (HiddenClockResetEnable dom)
    => Signal dom Concat4Input
    -> Signal dom Concat4Output
concat4 input = do
  -- A Feedback
  axiInputFeedbackA <- do
    ready <- ready_i * (boolToBit <$> (state .==. pure A))
    pure AxiBackward {..}
  -- B Feedback
  axiInputFeedbackB <- do
    ready <- ready_i * (boolToBit <$> (state .==. pure B))
    pure AxiBackward {..}
  -- C Feedback
  axiInputFeedbackC <- do
    ready <- ready_i * (boolToBit <$> (state .==. pure C))
    pure AxiBackward {..}
  -- D Feedback
  axiInputFeedbackD <- do
    ready <- ready_i * (boolToBit <$> (state .==. pure D))
    pure AxiBackward {..}
  axiOutput <- do
    valid <- valid_o 
    _data <- mux4
      <$> state
      <*> (input <&> (.axiInputA) <&> (._data))
      <*> (input <&> (.axiInputB) <&> (._data))
      <*> (input <&> (.axiInputC) <&> (._data))
      <*> (input <&> (.axiInputD) <&> (._data))
    last <- input <&> (.axiInputD) <&> (.last)
    pure AxiForward {..}
  pure Concat4Output {..}
  where
    valid_o = mux4
      <$> state
      <*> (input <&> (.axiInputA) <&> (.valid))
      <*> (input <&> (.axiInputB) <&> (.valid))
      <*> (input <&> (.axiInputC) <&> (.valid))
      <*> (input <&> (.axiInputD) <&> (.valid))
    slaveWrite = valid_o * ready_i
    ready_i = input <&> (.axiOutputFeedback) <&> (.ready)

    state = register A $ nextState
      <$> state
      <*> slaveWrite
      <*> (input <&> (.axiInputA) <&> (.last))
      <*> (input <&> (.axiInputB) <&> (.last))
      <*> (input <&> (.axiInputC) <&> (.last))
      <*> (input <&> (.axiInputD) <&> (.last))
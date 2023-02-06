{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}

module SunPhy.Bypass (bypass, BypassInput(..), BypassOutput(..)) where

import Clash.Prelude hiding (last)
import SunPhy.AXI
import Data.Functor ((<&>))

-- A Bypass block is very simple :
-- It takes data in through an AXI stream interface
-- and outputs it unchanged through another AXI stream interface
--
--                  ┏━━━━━━━━━━━━┓
--                  ┃   Bypass   ┃
--         ready_o <┨           <┨  ready_i
--         valid_i  ┠>           ┠> valid_o
--          data_i  ┠>           ┠> data_o
--          last_i  ┠>           ┠> last_o
--                  ┗━━━━━━━━━━━━┛
-- 
-- The challenge resides in the fact that no combinatory path should exist between
-- "left" (master) and "right" (slave) sides therefore everything must be
-- passed through registers.
-- The problem is that when the slave lowers its ready signal (right interface),
-- the left ready (for the master) will still be high for
-- a clock cycle and the master will be able to write to it.
-- The Bypass block has to buffer this incoming data and then output it


data BypassState = Idle -- No data in the buffer
-- There's data in the buffer, but it's coming out as new data is coming in
           | Pipe 
-- Data has accumulated in the input buffer,
-- it must come out before new data can be accepted
           | Buff 
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX

data BypassInput a = BypassInput
    { axiInput :: AxiForward a
    , axiOutputFeedback :: AxiBackward
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data BypassOutput a = BypassOutput
    { axiInputFeedback :: AxiBackward
    , axiOutput :: AxiForward a
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)


bypass
    :: forall dom a . HiddenClockResetEnable dom
    => Num a
    => NFDataX a
    => Signal dom (BypassInput a)
    -> Signal dom (BypassOutput a)
bypass input = do
  -- Declare the outputs
  -- AXI Input feedback
  axiInputFeedback <- do
    ready <- ready_o
    pure AxiBackward {..}
  -- AXI output
  axiOutput <- do
    valid <- valid_o
    _data <- mux (state ./=. pure Buff) a b
    last <- boolToBit <$> (
      state .==. pure Pipe
      .&&. slaveWrite .==. 1
      .&&. lastFlag .==. pure 1)
    pure AxiForward {..}
  pure BypassOutput {..}
  where
    slaveWrite = (input <&> (.axiOutputFeedback) <&> (.ready)) * valid_o
    masterWrite = ready_o * (input <&> (.axiInput) <&> (.valid))

    nextState :: BypassState -> Bit -> Bit -> BypassState
    --        ┌state
    --        │      ┌masterWrite
    --        │      │ ┌slaveWrite
    -- Idle   │      │ │
    nextState Idle 1 0 = Pipe -- Data in only
    -- Piped
    nextState Pipe 1 0 = Buff -- Bufferize the data
    nextState Pipe 0 1 = Idle -- End of incoming data (data out only)
    -- Buffer
    nextState Buff 0 1 = Pipe -- Data out (return to normal)
    -- Otherwise (no change)
    nextState x _ _ = x

    state = register Idle $ nextState
      <$> state
      <*> masterWrite
      <*> slaveWrite

    a = register 0 nextA
    nextA = mux
      (bitToBool <$> masterWrite)
      (input <&> (.axiInput) <&> (._data))
      a

    b = register 0 nextB
    nextB = mux (bitToBool <$> masterWrite) a b

    -- Last management
    -- A last flag is raised whenever the last_i input is high
    lastFlag = register (0 :: Bit) $ nextLastFlag
      <$> state
      <*> (input <&> (.axiInput) <&> (.last))
      <*> lastFlag

    nextLastFlag :: BypassState -> Bit -> Bit -> Bit
    --           ┌state
    --           │    ┌last_i
    --           │    │ ┌lastFlag
    -- Idle      │    │ │
    nextLastFlag Idle _ _ = 0
    nextLastFlag _    1 _ = 1
    nextLastFlag _    _ x = x

    -- Outputs
    ready_i_reg = register 0 (input <&> (.axiOutputFeedback) <&> (.ready))
    ready_o = boolToBit <$> (ready_i_reg .==. 1 .&&. lastFlag .==. 0)
    valid_o = boolToBit <$> (state ./=. pure Idle)

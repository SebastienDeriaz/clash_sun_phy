module SunPhy.Bypass (bypass) where

import Clash.Prelude

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
--                  ┃            ┃
--                  ┗━━━━━━━━━━━━┛
-- 
-- The challenge resides in the fact that no combinatory path should exist between
-- "left" and "right" sides. Therefore everything must be passed through registers.
-- The problem is that when the slave lowers its ready signal (right interface),
-- the left ready (for the master) will still be on for a clock cycle and the master
-- will be able to write to it.
-- The Bypass block has to buffer this incoming data and then output it


data State = Idle -- No data in the buffer
           | Piped -- There's data in the buffer, but it's coming out as new data is coming in
           | Buffer -- Data has accumulated in the input buffer, it must come out before new data can be accepted
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX


bypass
    :: forall dom . HiddenClockResetEnable dom
    => Signal dom Bit -- valid_i
    -> Signal dom Bit -- data_i
    -> Signal dom Bit -- last_i
    -> Signal dom Bit -- ready_i
    -> Signal dom (Bit, Bit, Bit, Bit) -- valid_o, data_o, last_o, ready_o
bypass valid_i data_i last_i ready_i = bundle(valid_o, data_o, last_o, ready_o)
  where    
    slaveWrite = ready_i * valid_o
    masterWrite = ready_o * valid_i

    nextState :: State -> Bit -> Bit -> State
    --        ┌state
    --        │      ┌masterWrite
    --        │      │ ┌slaveWrite
    -- Idle   │      │ │
    nextState Idle   1 0 = Piped -- Data in only
    -- Piped
    nextState Piped  1 0 = Buffer -- Bufferize the data
    nextState Piped  0 1 = Idle -- End of incoming data (data out only)
    -- Buffer
    nextState Buffer 0 1 = Piped -- Data out (return to normal)
    -- Otherwise (no change)
    nextState x      _ _ = x

    state = register (Idle) $ nextState <$> state <*> masterWrite <*> slaveWrite

    a = register (0 :: Bit) nextA
    nextA = mux (bitToBool <$> masterWrite) data_i a

    b = register (0 :: Bit) nextB
    nextB = mux (bitToBool <$> masterWrite) a b

    -- Last management
    -- A last flag is raised whenever the last_i input is high
    lastFlag = register (0 :: Bit) (nextLastFlag <$> state <*> last_i <*> lastFlag)

    nextLastFlag :: State -> Bit -> Bit -> Bit
    --           ┌state
    --           │    ┌last_i
    --           │    │ ┌lastFlag
    -- Idle      │    │ │
    nextLastFlag Idle _ _ = 0
    nextLastFlag _    1 _ = 1
    nextLastFlag _    _ x = x

    -- Outputs
    ready_i_reg = register (0 :: Bit) ready_i
    ready_o = boolToBit <$> (ready_i_reg .==. 1 .&&. lastFlag .==. 0)
    valid_o = boolToBit <$> (state ./=. pure Idle)
    data_o = mux (state ./=. pure Buffer) a b
    last_o = boolToBit <$> (state .==. pure Piped .&&. slaveWrite .==. 1 .&&. lastFlag .==. pure 1)

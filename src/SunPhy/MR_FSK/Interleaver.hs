{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}

module SunPhy.MR_FSK.Interleaver where


-- The interleaver takes 2x16 bits and places them inside an array
-- The output is this array traversed with a different index from writing
-- The data is processed 2 bits at a time
--
-- The input data is numbered 0 through 31 and written in the array
-- left to right, then top to bottom
-- 
-- The output data is read bottom to top, right to left
-- Array :
--  
--   0,1    2,3    4,5    6,7
--   8,9   10,11  12,13  14,15
--  16,17  18,19  20,21  22,23
--  24,25  26,27  28,29  30,31
--  
--  The output then becomes :
--  
--  30,31,22,23,14,15,6,7,28,29,20,21,12,13,4,5,26,27,18,19,10,11,2,3,24,25,16,17,8,9,0,1
--
-- Note that each index is a singular bit

import Clash.Prelude
import SunPhy.Bypass
import SunPhy.AXI
import Data.Functor ((<&>))


-- State machine
data State = Idle
            | Write
            | Read
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX



nextState :: State -> Bit -> Bit -> State
--        ┌state
--        │     ┌valid_i 
--        │     │ ┌counterEnd
-- Idle   │     │ │   
nextState Idle  0 _ = Idle
nextState Idle  1 _ = Write
-- Write
nextState Write _ 0 = Write
nextState Write _ 1 = Read
-- Read
nextState Read  _ 0 = Read
nextState Read  _ 1 = Idle


nextCounter :: State -> Bit -> Bit -> Bit -> Bit -> Unsigned 5 -> Unsigned 5
--          ┌state
--          │     ┌valid_i 
--          │     │ ┌ready_o
--          │     │ │ ┌counterEnd
--          │     │ │ │ ┌ready_i      
--          │     │ │ │ │ ┌counter              
-- Idle     │     │ │ │ │ │
nextCounter Idle  1 1 _ _ _ = 1
nextCounter Idle  _ _ _ _ _ = 0
nextCounter Write _ _ 1 _ _ = 0
nextCounter Write 1 1 _ _ x = x + 1
nextCounter Write _ _ _ _ x = x
nextCounter Read  _ _ 1 _ _ = 0
nextCounter Read  _ _ _ 1 x = x + 1
nextCounter Read  _ _ _ 0 x = x

nextBuffer :: State -> Bit -> Bit -> Unsigned 5 -> Bit -> BitVector 32 -> BitVector 32
-- state, masterWrite, slaveWrite, valid_i, ready_o, counter, data, buffer
--         ┌state
--         │     ┌masterWrite 
--         │     │ ┌slaveWrite
--         │     │ │ ┌counter
--         │     │ │ │  ┌data_i      
--         │     │ │ │  │ ┌buffer              
-- Idle    │     │ │ │  │ │
nextBuffer Idle  1 _ i  1 b = setBit b (fromEnum i)
nextBuffer Write 1 _ i  1 b = setBit b (fromEnum i)
nextBuffer Read  _ 1 31 _ _ = 0
nextBuffer _     _ _ _  _ b = b

ready' :: State -> Bit
ready' Idle = 1
ready' Write = 1
ready' Read = 0

nextLastStore :: State -> Bit -> Bit -> Bit
--            ┌state
--            │     ┌valid_i 
--            │     │ ┌ready_o
-- Idle       │     │ │ 
nextLastStore Write 1 _ = 1
nextLastStore Idle  _ _ = 0
nextLastStore _     _ x = x



outputIndex :: Unsigned 5 -> Unsigned 5
-- counter
outputIndex counter = output
  where
    lsbIndex = (counter .&. 1)
    -- t = (15 - 4 * np.mod(k, 4) - np.floor(k / 4)).astype(int)
    -- new[t] = old[k], but it also works the other way around new[k] = old[t]
    k = shiftR counter 1
    t = 15 - 4 * (mod k 4) - (shiftR k 2)
    output = (shiftL t 1) .|. lsbIndex

interleaver
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Bit -- bypass
  -> Signal dom Bit -- valid_i
  -> Signal dom Bit -- data_i
  -> Signal dom Bit -- last_i
  -> Signal dom Bit -- ready_i
  -> Signal dom (Bit, Bit, Bit, Bit) -- ready_o, data_o, valid_o, last_o, test
interleaver bp valid_i data_i last_i ready_i = bundle(ready_o, data_o, valid_o, last_o)
  where
    counter = register (0 :: Unsigned 5) $ nextCounter <$> state <*> valid_i <*> ready_o <*> (boolToBit <$> counterEnd) <*> ready_i <*> counter
    counterEnd = counter .==. (pure 31)

    lastStore = register (0 :: Bit) (nextLastStore <$> state <*> last_i <*> lastStore)

    slaveWrite = boolToBit <$> ((bitToBool <$> ready_i) .&&. (bitToBool <$> valid_out))
    masterWrite = boolToBit <$> ((bitToBool <$> ready_out) .&&. (bitToBool <$> valid_i))

    state = register Idle $ nextState <$> state <*> valid_i <*> (boolToBit <$> counterEnd)

    buffer = register (0 :: BitVector 32) $ nextBuffer <$> state <*> masterWrite <*> slaveWrite <*> counter <*> data_i <*> buffer

    output = boolToBit <$> (testBit <$> buffer <*> (fromEnum <$> (outputIndex <$> counter)))

    ready_out = ready' <$> state
    valid_out = boolToBit <$> (state .==. pure Read)


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
    -- Outputs
    bpb = bitToBool <$> bp
    ready_o = mux
      bpb
      (bypassOutput <&> (.axiInputFeedback) <&> (.ready))
      ready_out
      
    valid_o = mux
      bpb
      (bypassOutput <&> (.axiOutput) <&> (.valid))
      valid_out

    data_o = mux
      bpb
      (bypassOutput <&> (.axiOutput) <&> (._data))
      (mux (state .==. pure Read) output (pure 0))

    last_o = mux
      bpb
      (bypassOutput <&> (.axiOutput) <&> (.last))
      (boolToBit <$> (state .==. pure Read .&&. (lastStore .==. 1) .&&. counterEnd))
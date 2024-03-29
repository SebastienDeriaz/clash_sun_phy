{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}

module SunPhy.MR_FSK.FEC where


import Clash.Prelude hiding (foldr)
import Data.Foldable (foldr)

import SunPhy.MR_FSK.FECEncoder (fecEncoder, FecEncoderState)

import SunPhy.Bypass (bypass, BypassInput(..), BypassOutput(..))
import SunPhy.AXI
import Data.Functor ((<&>))

tailVec :: (BitVector 3) -> Bit -> BitVector 3
--      ┌m
--      │     ┌phyFSKFECScheme
-- Idle │     │ 
tailVec _     0 = 0b000
tailVec 0b000 _ = 0b000
tailVec 0b001 _ = 0b001
tailVec 0b010 _ = 0b011
tailVec 0b011 _ = 0b010
tailVec 0b100 _ = 0b111
tailVec 0b101 _ = 0b110
tailVec 0b110 _ = 0b100
tailVec 0b111 _ = 0b101

-- State machine
data State = Idle
           | Data
           | Tail
           | Pad
           | End
           | Last
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX

-- state bypass ready_o encoderValid_i last_i tailCounterEnd padCounterEnd
f_nextState :: State -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> State
--          ┌state
--          │    ┌bypass
--          │    │ ┌ready_o
--          │    │ │ ┌encoderValid_i
--          │    │ │ │ ┌last_i
--          │    │ │ │ │ ┌tailCounterEnd
--          │    │ │ │ │ │ ┌padCounterEnd
-- Idle     │    │ │ │ │ │ │ 
f_nextState _    1 _ _ _ _ _ = Idle
-- Idle
f_nextState Idle _ 1 1 _ _ _ = Data
f_nextState Idle _ _ _ _ _ _ = Idle
-- Data
f_nextState Data _ 1 1 1 _ _ = Tail
f_nextState Data _ _ _ _ _ _ = Data
-- Tail
f_nextState Tail _ 1 1 _ 1 _ = Pad
f_nextState Tail _ _ _ _ _ _ = Tail
-- Pad
f_nextState Pad  _ 1 1 _ _ 1 = End
f_nextState Pad  _ _ _ _ _ _ = Pad
-- End
f_nextState End  _ _ _ _ _ _ = Last
-- Last
f_nextState Last _ _ _ _ _ _ = Idle





nextTailCounter :: State -> Bit -> Bit -> Unsigned 2 -> Unsigned 2
-- State, tailCounterEnd, encoderReady_o, counter
--              ┌state
--              │    ┌tailCounterEnd
--              │    │ ┌encoderReady_o
--              │    │ │ ┌counter
-- Idle         │    │ │ │ 
nextTailCounter Tail 0 1 x = x + 1
nextTailCounter Tail _ _ x = x
nextTailCounter _    _ _ _ = 0


encoderInput :: State -> Bit -> Bit -> Bit -> Bit
encoderInput state data_i tail pad = case state of
    Idle -> data_i
    Data -> data_i
    Tail -> tail
    Pad  -> pad
    End  -> data_i
    Last -> data_i



-- 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1
pad_bits :: BitVector 13
pad_bits = 0b1101000011010

nextPadCounter :: State -> Bit -> Bit -> Unsigned 4 -> Unsigned 4
-- State, padCounterEnd, encoderReady_o, state
--             ┌state
--             │   ┌padCounterEnd
--             │   │ ┌encoderReady_o
--             │   │ │ ┌counter
-- Idle        │   │ │ │ 
nextPadCounter Pad 0 1 x = x + 1
nextPadCounter Pad _ _ x = x
nextPadCounter _   _ _ _ = 0

padCounterMax :: Bool -> Unsigned 4
--            ┌evenNBytes
--            │ 
padCounterMax True  = 12 -- even -> 13 bits
padCounterMax False = 4  -- odd  -> 5 bits

encoderValid_i :: State -> Bit -> Bit
--             ┌state
--             │    ┌valid_i
--             │    │ 
encoderValid_i Idle x = x
encoderValid_i Data x = x
encoderValid_i _    _ = 1

f_nextBitCounter :: State -> Bit -> Bit -> Unsigned 4 -> Unsigned 4
-- State, ready_o, valid_i, counter
--               ┌state
--               │    ┌ready_o
--               │    │ ┌valid_i
--               │    │ │ ┌counter
--               │    │ │ │ 
f_nextBitCounter Data 1 1 x = x + 1
f_nextBitCounter Idle 1 1 _ = 1
f_nextBitCounter Data _ _ x = x
f_nextBitCounter _    _ _ x = x

fec
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Bit -- bypass
  -> Signal dom Bit -- phyFSKFECScheme : 1 -> RSC, 0 -> NRNSC
  -> Signal dom Bit -- valid_i
  -> Signal dom Bit -- data_i
  -> Signal dom Bit -- last_i
  -> Signal dom Bit -- ready_i
  -> Signal dom (Bit, Bit, Bit, Bit) -- ready_o, valid_o, data_o, last_o
fec bp phyFSKFECScheme valid_i data_i last_i ready_i = bundle(ready_o, valid_o, data_o, last_o)
  where
    state = register (Idle :: State) nextState
    nextState = f_nextState <$> state <*> bp <*> encoderReady_o <*> encoderValid_i' <*> last_i <*> tailCounterEnd <*> padCounterEnd

    -- Bit counter (to determine if there's an even or odd number of bytes)
    bitCounter = register (0 :: Unsigned 4) (f_nextBitCounter <$> state <*> encoderReady_o <*> valid_i <*> bitCounter)
    -- Even number of bytes when the 4th bit of bitCounter is low
    evenNBytes = not <$> (testBit <$> bitCounter <*> 3)
    
    -- Tail counter
    tailCounter = register (0 :: Unsigned 2) (nextTailCounter <$> state <*> tailCounterEnd <*> encoderReady_o <*> tailCounter)
    tailCounterEnd = boolToBit <$> (tailCounter .==. 2)

    -- Pad counter (4->0 or 12->0)
    padCounter = register (0 :: Unsigned 4) (nextPadCounter <$> state <*> padCounterEnd <*> encoderReady_o <*> padCounter)
    padCounterEnd = boolToBit <$> (padCounter .==. (padCounterMax <$> evenNBytes))

    pad = boolToBit <$> (testBit pad_bits <$> (fromEnum <$> padCounter))
    
    tail = boolToBit <$> (testBit <$> (tailVec <$> m <*> phyFSKFECScheme) <*> (fromEnum <$> tailCounter))

    encoderValid_i' = encoderValid_i <$> state <*> valid_i
    encoderInput' = encoderInput <$> state <*> data_i <*> tail <*> pad

    mReg = register (0 :: BitVector 3) nextMReg
    nextMReg = mux (state .==. pure Data .&&. last_i .==. 1) m mReg
    (m, encoderReady_o, encoderData_o, encoderValid_o, encoderState) = unbundle $ fecEncoder phyFSKFECScheme ready_i encoderValid_i' encoderInput'

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
      encoderReady_o
    valid_o = mux
      bpb
      (bypassOutput <&> (.axiOutput) <&> (.valid))
      encoderValid_o
    data_o  = mux
      bpb
      (bypassOutput <&> (.axiOutput) <&> (._data))
      encoderData_o
    last_o  = mux
      bpb
      (bypassOutput <&> (.axiOutput) <&> (.last))
      (boolToBit <$> (state .==. pure Last))







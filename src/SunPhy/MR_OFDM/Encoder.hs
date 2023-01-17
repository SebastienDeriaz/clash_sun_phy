{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}

module SunPhy.MR_OFDM.Interleaver where

import Clash.Prelude hiding (foldr, B0, B1)
import Data.Foldable (foldr)
import SunPhy.MR_OFDM.Constants
import SunPhy.Bypass (bypass)

-- Rate
-- 0 : 1/2
-- 1 : 3/4

-- Input data
--   X0, X1, X2
-- Endoded data
--   (A0, B0), (A1, B1), (A2, B2)
-- Rate 1/2
--   A0, B0, A1, B1, A2, B2
-- Rate 3/4
--   A0, B0, A1, B2

data State = Idle
           | A0
           | B0
           | A1
           | B1
           | A2
           | B2
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX

nextState :: State -> Bit -> Bit -> Bit -> Bit -> State
--        ┌state
--        │    ┌rate 
--        │    │ ┌masterWrite
--        │    │ │ ┌slaveWrite
--        │    │ │ │ ┌is_last      
-- Idle   │    │ │ │ │
nextState Idle _ 0 _ _ = Idle
nextState Idle _ 1 _ _ = A0
-- A0
nextState A0   _ _ 1 _ = B0
nextState A0   _ _ 0 _ = A0
-- B0
nextState B0   _ 1 1 _ = A1
nextState B0   _ 0 0 _ = B0
nextState B0   _ _ _ _ = B0
-- A1
nextState A1   0 _ 1 _ = B1
nextState A1   1 1 1 _ = B2
nextState A1   _ _ _ _ = A1
-- A1WM (wait master), rate will always be 1
nextState B1   _ 1 1 _ = A2
nextState B1   _ _ _ _ = B1

nextState A2   _ _ 1 _ = B2
nextState A2   _ _ 0 _ = A2

nextState B2   _ _ 1 1 = Idle
nextState B2   _ _ 1 0 = A0
nextState B2   _ _ _ _ = B2

ready :: State -> Bit -> Bit -> Bit
-- counter, rate, buffered
--    ┌state
--    │    ┌rate 
--    │    │ ┌buffered
--    │    │ │
ready Idle _ _ = 1
ready B0   _ _ = 1
ready A1   1 _ = 1
ready B1   _ _ = 1
ready B2   _ _ = 1
ready _    _ _ = 0

valid :: State -> Bit
--    ┌state
--    │
valid Idle = 0
valid _    = 1

data_out :: State -> Bit -> Bit -> Bit
--       ┌state
--       │    ┌a 
--       │    │ ┌b
--       │    │ │
data_out Idle a _ = a
data_out A0   a _ = a
data_out A1   a _ = a
data_out A2   a _ = a
data_out B0   _ b = b
data_out B1   _ b = b
data_out B2   _ b = b


nextIsLast :: State -> Bit -> Bit -> Bit 
--         ┌state
--         │    ┌last_i 
--         │    │ ┌is_last
-- Idle    │    │ │
nextIsLast Idle _ _ = 0
nextIsLast _    1 _ = 1
nextIsLast _    _ x = x

last :: State -> Bit -> Bit
--   ┌state
--   │  ┌is_ast 
--   │  │ 
last B2 1 = 1
last _  _ = 0


nextBuffered :: State -> Bit -> Bit -> Bit -> Bit
--           ┌state
--           │    ┌slaveWrite 
--           │    │ ┌masterWrite
--           │    │ │ ┌buffered
-- Idle      │    │ │ │
nextBuffered Idle _ _ _ = 0 -- no change
nextBuffered _    0 1 0 = 1
nextBuffered _    1 0 1 = 0
nextBuffered _    _ _ x = x

encoder
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Bit -- rate
  -> Signal dom Bit -- valid_i
  -> Signal dom Bit -- data_i
  -> Signal dom Bit -- last_i
  -> Signal dom Bit -- ready_i
  -> Signal dom (Bit, Bit, Bit, Bit) -- ready_o, data_o, valid_o, last_o
encoder rate valid_i data_i last_i ready_i = bundle(ready_o, data_o, valid_o, last_o)
  where
    masterWrite = ready_o * valid_i
    slaveWrite = ready_i * valid_o

    -- When the slave blocks the stream but the ready_out (master) is still one, the data needs to be buffered
    -- When the slave is ready again, the current data is used first, then the buffered data and
    -- finally the new data can be read
    buffered = register (0 :: Bit) (nextBuffered <$> state <*> slaveWrite <*> masterWrite <*> buffered)
    inputBuffer = register (0 :: Bit) nextInputBuffer
    nextInputBuffer = (mux (masterWrite .==. 1) data_i inputBuffer)

    isLast = register (0 :: Bit) (nextIsLast <$> state <*> last_i <*> isLast)

    state = register (Idle) (nextState <$> state <*> rate <*> masterWrite <*> slaveWrite <*> isLast)
    ready_base = ready <$> state <*> rate <*> buffered
    ready_o = mux (bitToBool <$> buffered) 0 (ready_base)


    rst = boolToBit <$> (state .==. pure Idle .&&. isLast .==. 1)

    encoderValid = boolToBit <$> (ready_base .==. 1 .&&. valid_i .==. 1 .&&. ready_i .==. 1)

    encoderData = mux (buffered .==. 1) inputBuffer data_i
    (a, b) = unbundle $ convolutionalEncoder encoderData rst encoderValid

    -- Outputs
    valid_o = valid <$> state
    data_o = data_out <$> state <*> a <*> b
    last_o = boolToBit <$> (state .==. pure B2 .&&. isLast .==. 1)



newM :: BitVector 6 -> Bit -> Bit -> Bit -> BitVector 6
--   ┌m
--   │ ┌valid_i 
--   │ │ ┌input
--   │ │ │ ┌rst
--   │ │ │ │
newM _ _ _ 1 = 0
newM x 0 _ 0 = x
newM x 1 n 0 = (shiftR x 1) .|. (pack n ++# 0b00000)

convolutionalEncoder
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Bit -- Input data
  -> Signal dom Bit -- reset
  -> Signal dom Bit -- valid_i
  -> Signal dom (Bit, Bit)
convolutionalEncoder inp rst valid_i = bundle (a, b)
  where
    m = register (0 :: BitVector 6) (newM <$> m <*> valid_i <*> inp <*> rst)

    mBits i = boolToBit <$> (testBit <$> m <*> i)

    a = register (0 :: Bit) nextA
    nextA = mux (bitToBool <$> valid_i) (foldr (liftA2 xor) inp (mBits <$> [0, 1, 3, 4])) a

    b = register (0 :: Bit) nextB
    nextB = mux (bitToBool <$> valid_i) (foldr (liftA2 xor) inp (mBits <$> [0, 3, 4, 5])) b


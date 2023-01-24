{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module SunPhy.Scrambler where

import SunPhy.PN9 (pn9)

import Clash.Prelude


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
    => Signal dom Bit -- bypass
    -> Signal dom Bit -- ready_i
    -> Signal dom Bit -- valid_i
    -> Signal dom Bit -- data_i
    -> Signal dom Bit -- last_i
    -> Signal dom (BitVector 9) -- pn9_seed
    -> Signal dom (Bit, Bit, Bit, Bit) -- ready_o, valid_o, data_o, last_o
scrambler bypass ready_i valid_i data_i last_i pn9_seed = bundle (ready_o, valid_o, data_o, last_o)
  where
    slaveWrite = ready_i * valid_o
    masterWrite = ready_o * valid_i

    buffer = register (0 :: Unsigned 2) nextBuffer'
    nextBuffer' = nextBuffer <$> slaveWrite <*> masterWrite <*> buffer
    a_nb = buffer ./=. pure 2

    lastEnable = register (0 :: Bit) (nextLastEnable <$> lastEnable <*> buffer <*> last_i)

    pn9_next = masterWrite
    pn9_reset = last_o

    pn9_value :: Signal dom Bit
    (pn9_value, _) = unbundle $ pn9 pn9_seed pn9_next pn9_reset


    scrambledInput = xor <$> pn9_value <*> data_i

    a = register (0 :: Bit) nextA
    nextA = mux (bitToBool <$> masterWrite) scrambledInput a

    b = register (0 :: Bit) nextB
    nextB = mux (bitToBool <$> masterWrite) a b

    -- Outputs
    ready_o = register (0 :: Bit) ready_i
    valid_o = boolToBit <$> (buffer .>. pure 0)
    data_o = mux a_nb a b
    last_o = boolToBit <$> (buffer .==. pure 1 .&&. nextBuffer' .==. pure 0 .&&. lastEnable .==. pure 1)

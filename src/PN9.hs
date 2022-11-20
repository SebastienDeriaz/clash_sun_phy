module PN9 where

import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Bits

{-# NOINLINE topEntity #-}

pn9_next :: Bits a => a -> a
pn9_next a = (a `shiftR` 1) .|. next
  where
    next = ((a `shiftL` 3) `xor` (a `shiftL` 8)) .&. (bit 8)


reg_next :: Bits a => Bit -> a -> a
reg_next next previous
  | next == 0 = previous
  | next == 1 = pn9_next previous


topEntity :: HiddenClockResetEnable System
    => Signal System Bit -- next
    -> Signal System Bit -- output
topEntity request_next = msb <$> reg
  where
    reg = register (511 :: (BitVector 9)) ((reg_next <$> request_next) <*> reg)

testBench :: Signal System Bool
testBench = done
  where
    clk          = tbSystemClockGen (not <$> done)
    rst          = systemResetGen
    en           = enableGen
    testInput    = stimuliGenerator clk rst $(listToVecTH [1 :: Bit, 0, 0, 0, 0, 0, 0, 0])
    expectOutput = outputVerifier' clk rst $(listToVecTH [0 :: Bit, 0, 0, 0, 0, 0, 0, 0, 1])
    done         = expectOutput (withClockResetEnable clk rst en $ topEntity testInput)
    
    